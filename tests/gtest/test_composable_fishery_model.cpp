#include "../../inst/include/models/fisheries_models.hpp"

#include <gtest/gtest.h>

namespace {

template <typename Type>
class CountingComponent : public fims_popdy::ModelComponent<Type> {
 public:
  int initialize_count = 0;
  int prepare_count = 0;
  int evaluate_count = 0;
  int report_count = 0;
  size_t population_count = 0;

  std::string GetName() const override { return "counting"; }

  void Initialize(fims_popdy::ModelContext<Type> &context) override {
    initialize_count++;
    population_count = context.populations.size();
  }

  void Prepare(fims_popdy::ModelContext<Type> &context) override {
    prepare_count++;
    population_count = context.populations.size();
  }

  void Evaluate(fims_popdy::ModelContext<Type> &context) override {
    evaluate_count++;
    population_count = context.populations.size();
  }

  void Report(fims_popdy::ModelContext<Type> &context) override {
    report_count++;
    population_count = context.populations.size();
  }
};

}  // namespace

TEST(ComposableFisheryModel, RunsComponentsInLifecycleOrder) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto population = std::make_shared<fims_popdy::Population<double>>();
  auto component = std::make_shared<CountingComponent<double>>();

  model.populations.push_back(population);
  model.AddComponent(component);

  model.Initialize();
  model.Prepare();
  model.Evaluate();
  model.Report();

  EXPECT_EQ(component->initialize_count, 1);
  EXPECT_EQ(component->prepare_count, 1);
  EXPECT_EQ(component->evaluate_count, 1);
  EXPECT_EQ(component->report_count, 1);
  EXPECT_EQ(component->population_count, 1);
  ASSERT_EQ(model.GetComponentNames().size(), 1);
  EXPECT_EQ(model.GetComponentNames()[0], "counting");
}

TEST(ComposableFisheryModel, RunsComponentsByStage) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto component = std::make_shared<CountingComponent<double>>();
  model.AddComponent(component);

  model.RunComponents(fims_popdy::ModelComponentStage::kPrepare);
  model.RunComponents(fims_popdy::ModelComponentStage::kEvaluate);

  EXPECT_EQ(component->initialize_count, 0);
  EXPECT_EQ(component->prepare_count, 1);
  EXPECT_EQ(component->evaluate_count, 1);
  EXPECT_EQ(component->report_count, 0);
}

TEST(ComposableFisheryModel, RejectsNullComponents) {
  fims_popdy::ComposableFisheryModel<double> model;

  EXPECT_THROW(model.AddComponent(nullptr), std::invalid_argument);
  EXPECT_THROW(model.InsertComponent(0, nullptr), std::invalid_argument);
}

TEST(ComposableFisheryModel, EmplacesTypedComponents) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto component = model.EmplaceComponent<CountingComponent<double>>();

  EXPECT_EQ(model.GetComponents().size(), 1);
  EXPECT_EQ(model.GetComponentNames()[0], "counting");
  EXPECT_EQ(component.get(), model.GetComponents()[0].get());
}

TEST(ComposableFisheryModel, InsertsComponentsAtPositions) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto reset =
      std::make_shared<fims_popdy::ResetDerivedQuantitiesComponent<double>>();

  model.EmplaceComponent<CountingComponent<double>>();
  model.InsertComponent(0, reset);
  auto catchability =
      model.EmplaceComponentAt<
          fims_popdy::FleetCatchabilityTransformComponent<double>>(1);

  ASSERT_EQ(model.GetComponentNames().size(), 3);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.GetComponentNames()[1], "fleet_catchability_transform");
  EXPECT_EQ(model.GetComponentNames()[2], "counting");
  EXPECT_EQ(model.GetComponents()[0].get(), reset.get());
  EXPECT_EQ(model.GetComponents()[1].get(), catchability.get());
  EXPECT_THROW(model.InsertComponent(4, reset), std::out_of_range);
}

TEST(ComposableFisheryModel, ReplacesComponents) {
  fims_popdy::ComposableFisheryModel<double> model;
  model.EmplaceComponent<CountingComponent<double>>();
  model.EmplaceComponent<fims_popdy::ResetDerivedQuantitiesComponent<double>>();

  auto catchability =
      model.EmplaceComponentReplacement<
          fims_popdy::FleetCatchabilityTransformComponent<double>>(1);
  ASSERT_EQ(model.GetComponentNames().size(), 2);
  EXPECT_EQ(model.GetComponentNames()[0], "counting");
  EXPECT_EQ(model.GetComponentNames()[1], "fleet_catchability_transform");
  EXPECT_EQ(model.GetComponents()[1].get(), catchability.get());

  auto reset =
      std::make_shared<fims_popdy::ResetDerivedQuantitiesComponent<double>>();
  EXPECT_TRUE(model.ReplaceComponent("counting", reset));
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_FALSE(model.ReplaceComponent("missing", reset));
  EXPECT_THROW(model.ReplaceComponent(3, reset), std::out_of_range);
  EXPECT_THROW(model.ReplaceComponent(0, nullptr), std::invalid_argument);
  EXPECT_THROW(model.ReplaceComponent("counting", nullptr),
               std::invalid_argument);
}

TEST(ComposableFisheryModel, ClearsComponents) {
  fims_popdy::ComposableFisheryModel<double> model;
  model.EmplaceComponent<CountingComponent<double>>();

  model.ClearComponents();

  EXPECT_TRUE(model.GetComponents().empty());
  EXPECT_TRUE(model.GetComponentNames().empty());
}

TEST(ComposableFisheryModel, InspectsConstComponents) {
  fims_popdy::ComposableFisheryModel<double> model;
  model.EmplaceComponent<CountingComponent<double>>();
  const fims_popdy::ComposableFisheryModel<double> &const_model = model;

  EXPECT_EQ(const_model.GetComponents().size(), 1);
  EXPECT_EQ(const_model.GetComponentNames()[0], "counting");
}

TEST(ComposableFisheryModel, FindsComponentsByName) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto component = model.EmplaceComponent<CountingComponent<double>>();

  EXPECT_TRUE(model.HasComponent("counting"));
  EXPECT_FALSE(model.HasComponent("missing"));
  EXPECT_EQ(model.FindComponent("counting").get(), component.get());
  EXPECT_EQ(model.FindComponent("missing"), nullptr);

  const fims_popdy::ComposableFisheryModel<double> &const_model = model;
  EXPECT_TRUE(const_model.HasComponent("counting"));
  EXPECT_EQ(const_model.FindComponent("counting")->GetName(), "counting");
}

TEST(ComposableFisheryModel, FindsComponentsByType) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto component = model.EmplaceComponent<CountingComponent<double>>();
  model.EmplaceComponent<fims_popdy::ResetDerivedQuantitiesComponent<double>>();

  EXPECT_EQ(model.FindComponentAs<CountingComponent<double>>().get(),
            component.get());
  EXPECT_TRUE(model.HasComponentType<CountingComponent<double>>());
  EXPECT_EQ(model.FindComponentAs<
                fims_popdy::FleetCatchabilityTransformComponent<double>>(),
            nullptr);
  EXPECT_FALSE(model.HasComponentType<
               fims_popdy::FleetCatchabilityTransformComponent<double>>());

  const fims_popdy::ComposableFisheryModel<double> &const_model = model;
  EXPECT_EQ(const_model.FindComponentAs<CountingComponent<double>>()->GetName(),
            "counting");
  EXPECT_TRUE(const_model.HasComponentType<CountingComponent<double>>());
}

TEST(ComposableFisheryModel, RemovesComponentsByName) {
  fims_popdy::ComposableFisheryModel<double> model;
  model.EmplaceComponent<CountingComponent<double>>();
  model.EmplaceComponent<fims_popdy::ResetDerivedQuantitiesComponent<double>>();
  model.EmplaceComponent<CountingComponent<double>>();

  EXPECT_EQ(model.RemoveComponents("counting"), 2);

  ASSERT_EQ(model.GetComponentNames().size(), 1);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.RemoveComponents("missing"), 0);
}

TEST(ComposableFisheryModel, CopiesFleetsAndComponents) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto fleet = std::make_shared<fims_popdy::Fleet<double>>();
  model.fleets[fleet->GetId()] = fleet;
  model.EmplaceComponent<CountingComponent<double>>();

  fims_popdy::ComposableFisheryModel<double> copy(model);

  EXPECT_EQ(copy.fleets.size(), 1);
  EXPECT_EQ(copy.fleets.begin()->first, fleet->GetId());
  EXPECT_EQ(copy.GetComponentNames().size(), 1);
  EXPECT_EQ(copy.GetComponentNames()[0], "counting");
}

TEST(ComposableFisheryModel, StoresSharedModelNameInBase) {
  fims_popdy::ComposableFisheryModel<double> composable;
  fims_popdy::CatchAtAge<double> catch_at_age;
  fims_popdy::SurplusProduction<double> surplus_production;

  EXPECT_EQ(composable.name_m.find("model_"), 0);
  EXPECT_EQ(catch_at_age.name_m.find("caa_"), 0);
  EXPECT_EQ(surplus_production.name_m.find("sp_"), 0);

  fims_popdy::CatchAtAge<double> catch_at_age_copy(catch_at_age);
  EXPECT_EQ(catch_at_age_copy.name_m, catch_at_age.name_m);
}

TEST(ComposableFisheryModel, SetsModelIdentityFromBase) {
  fims_popdy::ComposableFisheryModel<double> model;

  model.SetModelIdentity("custom", "hybrid");

  EXPECT_EQ(model.model_type_m, "custom");
  EXPECT_EQ(model.name_m.find("hybrid_"), 0);
}

TEST(ComposableFisheryModel, AddsCommonFisheryComponents) {
  fims_popdy::ComposableFisheryModel<double> model;

  fims_popdy::AddCommonFisheryComponents(model);

  EXPECT_EQ(model.GetComponents().size(), 2);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.GetComponentNames()[1], "fleet_catchability_transform");
}

TEST(ComposableFisheryModel, AddsSurplusProductionPresetComponents) {
  fims_popdy::ComposableFisheryModel<double> model;

  fims_popdy::AddSurplusProductionComponents(model);

  EXPECT_EQ(model.model_type_m, "sp");
  EXPECT_EQ(model.GetComponents().size(), 4);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.GetComponentNames()[1], "fleet_catchability_transform");
  EXPECT_EQ(model.GetComponentNames()[2], "depletion_population_transform");
  EXPECT_EQ(model.GetComponentNames()[3], "surplus_production_dynamics");
}

TEST(ComposableFisheryModel, AddsAgeStructuredPreparationComponents) {
  fims_popdy::ComposableFisheryModel<double> model;

  fims_popdy::AddAgeStructuredPreparationComponents(model);

  EXPECT_EQ(model.model_type_m, "caa");
  EXPECT_EQ(model.GetComponents().size(), 3);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.GetComponentNames()[1], "fleet_catchability_transform");
  EXPECT_EQ(model.GetComponentNames()[2],
            "age_structured_population_transform");
}

TEST(ComposableFisheryModel, AddsCatchAtAgePresetComponents) {
  fims_popdy::ComposableFisheryModel<double> model;

  fims_popdy::AddCatchAtAgeComponents(model);

  EXPECT_EQ(model.model_type_m, "caa");
  EXPECT_EQ(model.GetComponents().size(), 4);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.GetComponentNames()[1], "fleet_catchability_transform");
  EXPECT_EQ(model.GetComponentNames()[2],
            "age_structured_population_transform");
  EXPECT_EQ(model.GetComponentNames()[3], "age_structured_dynamics");
}

TEST(ComposableFisheryModel, PresetComponentBuildersAreIdempotent) {
  fims_popdy::ComposableFisheryModel<double> catch_at_age;
  fims_popdy::AddCommonFisheryComponents(catch_at_age);
  fims_popdy::AddCatchAtAgeComponents(catch_at_age);
  fims_popdy::AddCatchAtAgeComponents(catch_at_age);

  EXPECT_EQ(catch_at_age.GetComponents().size(), 4);
  EXPECT_EQ(catch_at_age.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(catch_at_age.GetComponentNames()[1],
            "fleet_catchability_transform");
  EXPECT_EQ(catch_at_age.GetComponentNames()[2],
            "age_structured_population_transform");
  EXPECT_EQ(catch_at_age.GetComponentNames()[3], "age_structured_dynamics");

  fims_popdy::ComposableFisheryModel<double> surplus_production;
  fims_popdy::AddSurplusProductionComponents(surplus_production);
  fims_popdy::AddSurplusProductionComponents(surplus_production);

  EXPECT_EQ(surplus_production.GetComponents().size(), 4);
  EXPECT_EQ(surplus_production.GetComponentNames()[0],
            "reset_derived_quantities");
  EXPECT_EQ(surplus_production.GetComponentNames()[1],
            "fleet_catchability_transform");
  EXPECT_EQ(surplus_production.GetComponentNames()[2],
            "depletion_population_transform");
  EXPECT_EQ(surplus_production.GetComponentNames()[3],
            "surplus_production_dynamics");
}

TEST(ComposableFisheryModel, SurplusProductionUsesPresetComponents) {
  fims_popdy::SurplusProduction<double> model;

  EXPECT_EQ(model.model_type_m, "sp");
  EXPECT_EQ(model.GetComponents().size(), 4);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.GetComponentNames()[3], "surplus_production_dynamics");
}

TEST(ComposableFisheryModel, CatchAtAgeUsesPresetComponents) {
  fims_popdy::CatchAtAge<double> model;

  EXPECT_EQ(model.model_type_m, "caa");
  EXPECT_EQ(model.GetComponents().size(), 4);
  EXPECT_EQ(model.GetComponentNames()[0], "reset_derived_quantities");
  EXPECT_EQ(model.GetComponentNames()[3], "age_structured_dynamics");
}

TEST(ComposableFisheryModel, PresetModelsInheritComposableModel) {
  EXPECT_TRUE((std::is_base_of<fims_popdy::ComposableFisheryModel<double>,
                               fims_popdy::CatchAtAge<double>>::value));
  EXPECT_TRUE((std::is_base_of<fims_popdy::ComposableFisheryModel<double>,
                               fims_popdy::SurplusProduction<double>>::value));
}

TEST(ComposableFisheryModel, PresetModelsUseBasePopulationApi) {
  fims_popdy::CatchAtAge<double> catch_at_age;
  fims_popdy::SurplusProduction<double> surplus_production;
  auto population = std::make_shared<fims_popdy::Population<double>>();

  catch_at_age.AddPopulation(population->GetId());
  catch_at_age.GetPopulations().push_back(population);
  surplus_production.AddPopulation(population->GetId());
  surplus_production.GetPopulations().push_back(population);

  EXPECT_EQ(catch_at_age.GetPopulationIds().count(population->GetId()), 1);
  EXPECT_EQ(catch_at_age.GetPopulations().size(), 1);
  EXPECT_EQ(surplus_production.GetPopulationIds().count(population->GetId()),
            1);
  EXPECT_EQ(surplus_production.GetPopulations().size(), 1);

  const fims_popdy::CatchAtAge<double> &const_catch_at_age = catch_at_age;
  EXPECT_EQ(const_catch_at_age.GetPopulationIds().count(population->GetId()),
            1);
  EXPECT_EQ(const_catch_at_age.GetPopulations().size(), 1);
}

TEST(ComposableFisheryModel, RegistrationPreservesDerivedQuantityTags) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto population = std::make_shared<fims_popdy::Population<double>>();
  auto fleet = std::make_shared<fims_popdy::Fleet<double>>();

  model.RegisterPopulationDerivedQuantity(population->GetId(), "biomass", 1)
      .set_tag("population.biomass");
  model.RegisterFleetDerivedQuantity(fleet->GetId(), "index_expected", 1)
      .set_tag("fleet.index_expected");

  model.RegisterPopulationDerivedQuantity(population->GetId(), "biomass", 3);
  model.RegisterFleetDerivedQuantity(fleet->GetId(), "index_expected", 4);

  auto &population_dq =
      model.GetPopulationDerivedQuantities(population->GetId());
  auto &fleet_dq = model.GetFleetDerivedQuantities(fleet->GetId());

  EXPECT_EQ(population_dq["biomass"].size(), 3);
  EXPECT_EQ(population_dq["biomass"].get_tag(), "population.biomass");
  EXPECT_EQ(fleet_dq["index_expected"].size(), 4);
  EXPECT_EQ(fleet_dq["index_expected"].get_tag(), "fleet.index_expected");
}

TEST(ComposableFisheryModel, EnsureDerivedQuantitiesCreatesMaps) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto population = std::make_shared<fims_popdy::Population<double>>();
  auto fleet = std::make_shared<fims_popdy::Fleet<double>>();

  auto &population_dq =
      model.EnsurePopulationDerivedQuantities(population->GetId());
  auto &fleet_dq = model.EnsureFleetDerivedQuantities(fleet->GetId());

  EXPECT_TRUE(population_dq.empty());
  EXPECT_TRUE(fleet_dq.empty());
  EXPECT_NO_THROW(model.GetPopulationDerivedQuantities(population->GetId()));
  EXPECT_NO_THROW(model.GetFleetDerivedQuantities(fleet->GetId()));
}

TEST(ComposableFisheryModel, RequiredDerivedQuantityLookupDoesNotInsert) {
  std::map<std::string, fims::Vector<double>> derived_quantities;
  derived_quantities["biomass"] = fims::Vector<double>(2, 1.0);

  fims::Vector<double> &biomass =
      fims_popdy::GetRequiredDerivedQuantity(derived_quantities, "biomass");

  EXPECT_EQ(biomass.size(), 2);
  EXPECT_EQ(derived_quantities.count("biomass"), 1);
  EXPECT_THROW(
      fims_popdy::GetRequiredDerivedQuantity(derived_quantities, "missing"),
      std::out_of_range);
  EXPECT_EQ(derived_quantities.count("missing"), 0);
}

TEST(ComposableFisheryModel, RegistersOnlyAgeStructuredDerivedQuantities) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto population = std::make_shared<fims_popdy::Population<double>>();
  auto fleet = std::make_shared<fims_popdy::Fleet<double>>();

  population->n_ages = 2;
  population->n_years = 3;
  population->fleets.push_back(fleet);
  fleet->n_ages = population->n_ages;
  fleet->n_years = population->n_years;
  model.populations.push_back(population);
  model.fleets[fleet->GetId()] = fleet;

  fims_popdy::AddCatchAtAgeComponents(model);
  model.Initialize();

  EXPECT_EQ(population->n_fleets, 1);

  auto &population_dq =
      model.GetPopulationDerivedQuantities(population->GetId());
  EXPECT_EQ(population_dq.count("mortality_F"), 1);
  EXPECT_EQ(population_dq.count("numbers_at_age"), 1);
  EXPECT_EQ(population_dq.count("spawning_biomass"), 1);
  EXPECT_EQ(population_dq.count("expected_recruitment"), 1);
  EXPECT_EQ(population_dq.count("observed_catch"), 0);
  EXPECT_EQ(population_dq.count("fmsy"), 0);
  EXPECT_EQ(population_dq["mortality_F"].size(),
            population->n_years * population->n_ages);
  EXPECT_EQ(population_dq["numbers_at_age"].size(),
            (population->n_years + 1) * population->n_ages);

  auto &population_dims =
      model.GetPopulationDimensionInfo(population->GetId());
  EXPECT_EQ(population_dims.count("mortality_F"), 1);
  EXPECT_EQ(population_dims["mortality_F"].ndims, 2);
  EXPECT_EQ(population_dims["mortality_F"].dims[0], population->n_years);
  EXPECT_EQ(population_dims["mortality_F"].dims[1], population->n_ages);
  EXPECT_EQ(population_dims["mortality_F"].dim_names[0], "year");
  EXPECT_EQ(population_dims["mortality_F"].dim_names[1], "age");
  EXPECT_EQ(population_dims.count("observed_catch"), 0);

  auto &fleet_dq = model.GetFleetDerivedQuantities(fleet->GetId());
  EXPECT_EQ(fleet_dq.count("landings_numbers_at_age"), 1);
  EXPECT_EQ(fleet_dq.count("agecomp_expected"), 1);
  EXPECT_EQ(fleet_dq.count("landings_numbers_at_length"), 0);
  EXPECT_EQ(fleet_dq.count("lengthcomp_expected"), 0);
  EXPECT_EQ(fleet_dq.count("mean_log_q"), 0);

  auto &fleet_dims = model.GetFleetDimensionInfo(fleet->GetId());
  EXPECT_EQ(fleet_dims.count("landings_numbers_at_age"), 1);
  EXPECT_EQ(fleet_dims["landings_numbers_at_age"].ndims, 2);
  EXPECT_EQ(fleet_dims["landings_numbers_at_age"].dims[0],
            population->n_years);
  EXPECT_EQ(fleet_dims["landings_numbers_at_age"].dims[1],
            population->n_ages);
  EXPECT_EQ(fleet_dims.count("mean_log_q"), 0);
}

TEST(ComposableFisheryModel, RegistersOnlySurplusProductionDerivedQuantities) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto population = std::make_shared<fims_popdy::Population<double>>();
  auto fleet = std::make_shared<fims_popdy::Fleet<double>>();
  auto depletion =
      std::make_shared<fims_popdy::PellaTomlinsonDepletion<double>>();

  population->n_years = 3;
  population->depletion_module = depletion;
  population->fleets.push_back(fleet);
  fleet->n_years = population->n_years;
  model.populations.push_back(population);
  model.fleets[fleet->GetId()] = fleet;

  fims_popdy::AddSurplusProductionComponents(model);
  model.Initialize();

  EXPECT_EQ(population->n_fleets, 1);

  auto &population_dq =
      model.GetPopulationDerivedQuantities(population->GetId());
  EXPECT_EQ(population_dq.count("biomass"), 1);
  EXPECT_EQ(population_dq.count("observed_catch"), 1);
  EXPECT_EQ(population_dq.count("harvest_rate"), 1);
  EXPECT_EQ(population_dq.count("fmsy"), 1);
  EXPECT_EQ(population_dq.count("mortality_F"), 0);
  EXPECT_EQ(population_dq.count("numbers_at_age"), 0);
  EXPECT_EQ(population_dq["biomass"].size(), population->n_years + 1);
  EXPECT_EQ(population_dq["observed_catch"].size(), population->n_years);

  auto &population_dims =
      model.GetPopulationDimensionInfo(population->GetId());
  EXPECT_EQ(population_dims.count("biomass"), 1);
  EXPECT_EQ(population_dims["biomass"].ndims, 1);
  EXPECT_EQ(population_dims["biomass"].dims[0], population->n_years + 1);
  EXPECT_EQ(population_dims["biomass"].dim_names[0], "year");
  EXPECT_EQ(population_dims.count("mortality_F"), 0);

  auto &fleet_dq = model.GetFleetDerivedQuantities(fleet->GetId());
  EXPECT_EQ(fleet_dq.count("index_expected"), 1);
  EXPECT_EQ(fleet_dq.count("mean_log_q"), 1);
  EXPECT_EQ(fleet_dq.count("landings_numbers_at_age"), 0);
  EXPECT_EQ(fleet_dq["index_expected"].size(), population->n_years);

  auto &fleet_dims = model.GetFleetDimensionInfo(fleet->GetId());
  EXPECT_EQ(fleet_dims.count("index_expected"), 1);
  EXPECT_EQ(fleet_dims["index_expected"].dims[0], population->n_years);
  EXPECT_EQ(fleet_dims.count("mean_log_q"), 1);
  EXPECT_EQ(fleet_dims["mean_log_q"].dim_names[0], "value");
  EXPECT_EQ(fleet_dims.count("landings_numbers_at_age"), 0);
}

TEST(ComposableFisheryModel, AgeStructuredFleetPredictionAggregatesTotals) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto population = std::make_shared<fims_popdy::Population<double>>();
  auto fleet = std::make_shared<fims_popdy::Fleet<double>>();

  population->n_ages = 2;
  population->n_years = 1;
  population->n_fleets = 1;
  population->fleets.push_back(fleet);
  model.populations.push_back(population);
  model.fleets[fleet->GetId()] = fleet;

  auto &population_dq =
      model.GetPopulationDerivedQuantities()[population->GetId()];
  population_dq["total_landings_weight"] = fims::Vector<double>(1, 0.0);
  population_dq["total_landings_numbers"] = fims::Vector<double>(1, 0.0);

  auto &fleet_dq = model.GetFleetDerivedQuantities()[fleet->GetId()];
  fleet_dq["landings_weight_at_age"] = fims::Vector<double>({1.5, 2.5});
  fleet_dq["landings_numbers_at_age"] = fims::Vector<double>({3.0, 4.0});
  fleet_dq["landings_weight"] = fims::Vector<double>(1, 0.0);
  fleet_dq["landings_numbers"] = fims::Vector<double>(1, 0.0);
  fleet_dq["index_weight_at_age"] = fims::Vector<double>({5.0, 6.0});
  fleet_dq["index_numbers_at_age"] = fims::Vector<double>({7.0, 8.0});
  fleet_dq["index_weight"] = fims::Vector<double>(1, 0.0);
  fleet_dq["index_numbers"] = fims::Vector<double>(1, 0.0);

  fims_popdy::ModelContext<double> context = model.CreateContext();
  fims_popdy::AgeStructuredFleetPredictionComponent<double> component;
  component.CalculateLandings(context, population, 0, 0);
  component.CalculateLandings(context, population, 0, 1);
  component.CalculateIndex(context, population, 0, 0, 0);
  component.CalculateIndex(context, population, 1, 0, 1);

  EXPECT_DOUBLE_EQ(population_dq["total_landings_weight"][0], 4.0);
  EXPECT_DOUBLE_EQ(population_dq["total_landings_numbers"][0], 7.0);
  EXPECT_DOUBLE_EQ(fleet_dq["landings_weight"][0], 4.0);
  EXPECT_DOUBLE_EQ(fleet_dq["landings_numbers"][0], 7.0);
  EXPECT_DOUBLE_EQ(fleet_dq["index_weight"][0], 11.0);
  EXPECT_DOUBLE_EQ(fleet_dq["index_numbers"][0], 15.0);
}

TEST(ComposableFisheryModel, AgeStructuredFleetObservationBuildsExpectations) {
  fims_popdy::ComposableFisheryModel<double> model;
  auto fleet = std::make_shared<fims_popdy::Fleet<double>>();

  fleet->n_ages = 2;
  fleet->n_years = 1;
  fleet->n_lengths = 0;
  fleet->observed_index_units = "number";
  fleet->observed_landings_units = "weight";
  fleet->fleet_observed_landings_data_id_m = -999;
  fleet->fleet_observed_agecomp_data_id_m = -999;
  fleet->fleet_observed_lengthcomp_data_id_m = -999;
  model.fleets[fleet->GetId()] = fleet;

  auto &fleet_dq = model.GetFleetDerivedQuantities()[fleet->GetId()];
  fleet_dq["index_numbers_at_age"] = fims::Vector<double>({2.0, 6.0});
  fleet_dq["landings_numbers_at_age"] = fims::Vector<double>({3.0, 5.0});
  fleet_dq["agecomp_expected"] = fims::Vector<double>(2, 0.0);
  fleet_dq["agecomp_proportion"] = fims::Vector<double>(2, 0.0);
  fleet_dq["index_numbers"] = fims::Vector<double>({8.0});
  fleet_dq["index_weight"] = fims::Vector<double>({9.0});
  fleet_dq["index_expected"] = fims::Vector<double>(1, 0.0);
  fleet_dq["log_index_expected"] = fims::Vector<double>(1, 0.0);
  fleet_dq["landings_numbers"] = fims::Vector<double>({10.0});
  fleet_dq["landings_weight"] = fims::Vector<double>({11.0});
  fleet_dq["landings_expected"] = fims::Vector<double>(1, 0.0);
  fleet_dq["log_landings_expected"] = fims::Vector<double>(1, 0.0);

  fims_popdy::ModelContext<double> context = model.CreateContext();
  fims_popdy::AgeStructuredFleetObservationComponent<double> component;
  component.Evaluate(context);

  EXPECT_DOUBLE_EQ(fleet_dq["agecomp_expected"][0], 2.0);
  EXPECT_DOUBLE_EQ(fleet_dq["agecomp_expected"][1], 6.0);
  EXPECT_DOUBLE_EQ(fleet_dq["agecomp_proportion"][0], 0.25);
  EXPECT_DOUBLE_EQ(fleet_dq["agecomp_proportion"][1], 0.75);
  EXPECT_DOUBLE_EQ(fleet_dq["index_expected"][0], 8.0);
  EXPECT_DOUBLE_EQ(fleet_dq["landings_expected"][0], 11.0);
  EXPECT_EQ(fleet_dq.count("lengthcomp_expected"), 0);
  EXPECT_EQ(fleet_dq.count("lengthcomp_proportion"), 0);
  EXPECT_EQ(fleet_dq.count("landings_numbers_at_length"), 0);
  EXPECT_EQ(fleet_dq.count("index_numbers_at_length"), 0);
}
