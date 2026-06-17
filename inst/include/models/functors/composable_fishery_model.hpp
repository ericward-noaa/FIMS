/**
 * @file composable_fishery_model.hpp
 * @brief Defines a fishery model assembled from ordered model components.
 * @copyright This file is part of the NOAA, National Marine Fisheries Service
 * Fisheries Integrated Modeling System project. See LICENSE in the source
 * folder for reuse information.
 */
#ifndef FIMS_MODELS_COMPOSABLE_FISHERY_MODEL_HPP
#define FIMS_MODELS_COMPOSABLE_FISHERY_MODEL_HPP

#include <memory>
#include <string>

#include "fishery_model_base.hpp"

namespace fims_popdy {

template <typename Type>
void RegisterPopulationYearQuantity(
    ModelContext<Type> &context, uint32_t population_id,
    const std::string &name, size_t n_years) {
  context.RegisterPopulationDerivedQuantity(
      population_id, name, n_years,
      fims::Vector<int>({static_cast<int>(n_years)}),
      fims::Vector<std::string>({"year"}));
}

template <typename Type>
void RegisterPopulationAgeYearQuantity(
    ModelContext<Type> &context, uint32_t population_id,
    const std::string &name, size_t n_years, size_t n_ages) {
  context.RegisterPopulationDerivedQuantity(
      population_id, name, n_years * n_ages,
      fims::Vector<int>(
          {static_cast<int>(n_years), static_cast<int>(n_ages)}),
      fims::Vector<std::string>({"year", "age"}));
}

template <typename Type>
void RegisterPopulationScalarQuantity(ModelContext<Type> &context,
                                      uint32_t population_id,
                                      const std::string &name) {
  context.RegisterPopulationDerivedQuantity(
      population_id, name, 1, fims::Vector<int>({1}),
      fims::Vector<std::string>({"value"}));
}

template <typename Type>
void RegisterFleetYearQuantity(ModelContext<Type> &context, uint32_t fleet_id,
                               const std::string &name, size_t n_years) {
  context.RegisterFleetDerivedQuantity(
      fleet_id, name, n_years,
      fims::Vector<int>({static_cast<int>(n_years)}),
      fims::Vector<std::string>({"year"}));
}

template <typename Type>
void RegisterFleetAgeYearQuantity(ModelContext<Type> &context,
                                  uint32_t fleet_id,
                                  const std::string &name, size_t n_years,
                                  size_t n_ages) {
  context.RegisterFleetDerivedQuantity(
      fleet_id, name, n_years * n_ages,
      fims::Vector<int>(
          {static_cast<int>(n_years), static_cast<int>(n_ages)}),
      fims::Vector<std::string>({"year", "age"}));
}

template <typename Type>
void RegisterFleetLengthYearQuantity(ModelContext<Type> &context,
                                     uint32_t fleet_id,
                                     const std::string &name, size_t n_years,
                                     size_t n_lengths) {
  context.RegisterFleetDerivedQuantity(
      fleet_id, name, n_years * n_lengths,
      fims::Vector<int>(
          {static_cast<int>(n_years), static_cast<int>(n_lengths)}),
      fims::Vector<std::string>({"year", "length"}));
}

template <typename Type>
void RegisterFleetScalarQuantity(ModelContext<Type> &context,
                                 uint32_t fleet_id,
                                 const std::string &name) {
  context.RegisterFleetDerivedQuantity(
      fleet_id, name, 1, fims::Vector<int>({1}),
      fims::Vector<std::string>({"value"}));
}

/**
 * @brief Generic fishery model that delegates behavior to components.
 *
 * @details This class is the component-assembled model surface. Preset
 * model-family classes can add an ordered component stack, while new models
 * can be assembled directly from components.
 */
template <typename Type>
class ComposableFisheryModel : public FisheryModelBase<Type> {
 public:
  ComposableFisheryModel() : FisheryModelBase<Type>() {
    this->SetModelIdentity("composable", "model");
  }

  ComposableFisheryModel(const ComposableFisheryModel &other)
      : FisheryModelBase<Type>(other) {
    this->model_type_m = other.model_type_m;
  }

  virtual ~ComposableFisheryModel() {}

};

/**
 * @brief Component that resets all registered derived quantities.
 */
template <typename Type>
class ResetDerivedQuantitiesComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const { return "reset_derived_quantities"; }

  virtual void Prepare(ModelContext<Type> &context) {
    for (auto &population_entry : context.population_derived_quantities) {
      for (auto &quantity_entry : population_entry.second) {
        std::fill(quantity_entry.second.begin(), quantity_entry.second.end(),
                  static_cast<Type>(0.0));
      }
    }

    for (auto &fleet_entry : context.fleet_derived_quantities) {
      for (auto &quantity_entry : fleet_entry.second) {
        std::fill(quantity_entry.second.begin(), quantity_entry.second.end(),
                  static_cast<Type>(0.0));
      }
    }
  }
};

/**
 * @brief Component that prepares catchability values from log catchability.
 */
template <typename Type>
class FleetCatchabilityTransformComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const { return "fleet_catchability_transform"; }

  virtual void Initialize(ModelContext<Type> &context) {
    for (auto &fleet_entry : context.fleets) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_entry.second;
      if (fleet->log_q.size() == 0) {
        fleet->log_q.resize(1);
        fleet->log_q[0] = static_cast<Type>(0.0);
      }
      fleet->q.resize(fleet->log_q.size());
    }
  }

  virtual void Prepare(ModelContext<Type> &context) {
    for (auto &fleet_entry : context.fleets) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_entry.second;
      for (size_t i = 0; i < fleet->log_q.size(); i++) {
        fleet->q[i] = fims_math::exp(fleet->log_q[i]);
      }
    }
  }
};

/**
 * @brief Component that prepares age-structured population transformations.
 */
template <typename Type>
class AgeStructuredPopulationTransformComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const {
    return "age_structured_population_transform";
  }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> &population =
          context.populations[p];
      population->n_fleets = population->fleets.size();
      population->proportion_female.resize(population->n_ages);
      population->M.resize(population->n_years * population->n_ages);
      population->f_multiplier.resize(population->n_years);
      population->spawning_biomass_ratio.resize(population->n_years + 1);
    }

    for (auto &fleet_entry : context.fleets) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_entry.second;
      fleet->Fmort.resize(fleet->n_years);
    }
  }

  virtual void Prepare(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> &population =
          context.populations[p];

      for (size_t age = 0; age < population->n_ages; age++) {
        population->proportion_female[age] = static_cast<Type>(0.5);
      }

      for (size_t age = 0; age < population->n_ages; age++) {
        for (size_t year = 0; year < population->n_years; year++) {
          size_t i_age_year = age * population->n_years + year;
          population->M[i_age_year] =
              fims_math::exp(population->log_M[i_age_year]);
        }
      }

      for (size_t year = 0; year < population->n_years; year++) {
        population->f_multiplier[year] =
            fims_math::exp(population->log_f_multiplier[year]);
      }
    }

    for (auto &fleet_entry : context.fleets) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_entry.second;
      for (size_t year = 0; year < fleet->n_years; year++) {
        fleet->Fmort[year] = fims_math::exp(fleet->log_Fmort[year]);
      }
    }
  }
};

/**
 * @brief Component for age-structured total mortality calculations.
 */
template <typename Type>
class AgeStructuredMortalityComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const { return "age_structured_mortality"; }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      RegisterPopulationAgeYearQuantity(
          context, population->GetId(), "mortality_F", population->n_years,
          population->n_ages);
      RegisterPopulationAgeYearQuantity(
          context, population->GetId(), "mortality_M", population->n_years,
          population->n_ages);
      RegisterPopulationAgeYearQuantity(
          context, population->GetId(), "mortality_Z", population->n_years,
          population->n_ages);
      RegisterPopulationAgeYearQuantity(
          context, population->GetId(), "sum_selectivity",
          population->n_years, population->n_ages);
    }
  }

  void CalculateMortality(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &mortality_F =
        GetRequiredDerivedQuantity(population_dq, "mortality_F");
    fims::Vector<Type> &sum_selectivity =
        GetRequiredDerivedQuantity(population_dq, "sum_selectivity");
    fims::Vector<Type> &mortality_M =
        GetRequiredDerivedQuantity(population_dq, "mortality_M");
    fims::Vector<Type> &mortality_Z =
        GetRequiredDerivedQuantity(population_dq, "mortality_Z");

    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      Type selectivity = population->fleets[fleet]->selectivity->evaluate(
          population->ages[age], year);

      mortality_F[i_age_year] +=
          population->fleets[fleet]->Fmort[year] *
          population->f_multiplier[year] * selectivity;
      sum_selectivity[i_age_year] += selectivity;
    }

    mortality_M[i_age_year] = population->M[i_age_year];
    mortality_Z[i_age_year] = population->M[i_age_year] +
                              mortality_F[i_age_year];
  }
};

/**
 * @brief Component for age-structured abundance-at-age updates.
 */
template <typename Type>
class AgeStructuredNumbersComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const { return "age_structured_numbers"; }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      RegisterPopulationAgeYearQuantity(
          context, population->GetId(), "numbers_at_age",
          population->n_years + 1, population->n_ages);
      RegisterPopulationAgeYearQuantity(
          context, population->GetId(), "unfished_numbers_at_age",
          population->n_years + 1, population->n_ages);
    }
  }

  void CalculateInitialNumbersAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "numbers_at_age");
    numbers_at_age[i_age_year] = fims_math::exp(population->log_init_naa[age]);
  }

  void CalculateNumbersAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t i_agem1_yearm1, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "numbers_at_age");
    fims::Vector<Type> &mortality_Z =
        GetRequiredDerivedQuantity(population_dq, "mortality_Z");

    numbers_at_age[i_age_year] =
        numbers_at_age[i_agem1_yearm1] *
        fims_math::exp(-mortality_Z[i_agem1_yearm1]);

    if (age == (population->n_ages - 1)) {
      numbers_at_age[i_age_year] +=
          numbers_at_age[i_agem1_yearm1 + 1] *
          fims_math::exp(-mortality_Z[i_agem1_yearm1 + 1]);
    }
  }

  void CalculateUnfishedNumbersAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t i_agem1_yearm1, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &unfished_numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "unfished_numbers_at_age");

    unfished_numbers_at_age[i_age_year] =
        unfished_numbers_at_age[i_agem1_yearm1] *
        fims_math::exp(-population->M[i_agem1_yearm1]);

    if (age == (population->n_ages - 1)) {
      unfished_numbers_at_age[i_age_year] +=
          unfished_numbers_at_age[i_agem1_yearm1 + 1] *
          fims_math::exp(-population->M[i_agem1_yearm1 + 1]);
    }
  }
};

/**
 * @brief Component for age-structured biomass and spawning biomass calculations.
 */
template <typename Type>
class AgeStructuredBiomassComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const { return "age_structured_biomass"; }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      size_t year_count = population->n_years + 1;
      RegisterPopulationYearQuantity(context, population->GetId(), "biomass",
                                     year_count);
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "spawning_biomass", year_count);
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "unfished_biomass", year_count);
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "unfished_spawning_biomass", year_count);
    }
  }

  void CalculateBiomass(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &biomass =
        GetRequiredDerivedQuantity(population_dq, "biomass");
    fims::Vector<Type> &numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "numbers_at_age");
    biomass[year] +=
        numbers_at_age[i_age_year] *
        population->growth->evaluate(year, population->ages[age]);
  }

  void CalculateUnfishedBiomass(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &unfished_biomass =
        GetRequiredDerivedQuantity(population_dq, "unfished_biomass");
    fims::Vector<Type> &unfished_numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "unfished_numbers_at_age");
    unfished_biomass[year] +=
        unfished_numbers_at_age[i_age_year] *
        population->growth->evaluate(year, population->ages[age]);
  }

  void CalculateSpawningBiomass(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &spawning_biomass =
        GetRequiredDerivedQuantity(population_dq, "spawning_biomass");
    fims::Vector<Type> &numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "numbers_at_age");
    fims::Vector<Type> &proportion_mature_at_age =
        GetRequiredDerivedQuantity(population_dq, "proportion_mature_at_age");
    spawning_biomass[year] +=
        population->proportion_female[age] *
        numbers_at_age[i_age_year] *
        proportion_mature_at_age[i_age_year] *
        population->growth->evaluate(year, population->ages[age]);
  }

  void CalculateUnfishedSpawningBiomass(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &unfished_spawning_biomass =
        GetRequiredDerivedQuantity(population_dq,
                                   "unfished_spawning_biomass");
    fims::Vector<Type> &unfished_numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "unfished_numbers_at_age");
    fims::Vector<Type> &proportion_mature_at_age =
        GetRequiredDerivedQuantity(population_dq, "proportion_mature_at_age");
    unfished_spawning_biomass[year] +=
        population->proportion_female[age] *
        unfished_numbers_at_age[i_age_year] *
        proportion_mature_at_age[i_age_year] *
        population->growth->evaluate(year, population->ages[age]);
  }

  void CalculateSpawningBiomassRatio(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &spawning_biomass =
        GetRequiredDerivedQuantity(population_dq, "spawning_biomass");
    fims::Vector<Type> &unfished_spawning_biomass =
        GetRequiredDerivedQuantity(population_dq,
                                   "unfished_spawning_biomass");
    population->spawning_biomass_ratio[year] =
        spawning_biomass[year] / unfished_spawning_biomass[0];
  }

  Type CalculateSBPR0(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &proportion_mature_at_age =
        GetRequiredDerivedQuantity(population_dq, "proportion_mature_at_age");

    std::vector<Type> numbers_spr(population->n_ages, 1.0);
    Type phi_0 = 0.0;
    phi_0 += numbers_spr[0] * population->proportion_female[0] *
             proportion_mature_at_age[0] *
             population->growth->evaluate(0, population->ages[0]);
    for (size_t age = 1; age < (population->n_ages - 1); age++) {
      numbers_spr[age] =
          numbers_spr[age - 1] * fims_math::exp(-population->M[age]);
      phi_0 += numbers_spr[age] * population->proportion_female[age] *
               proportion_mature_at_age[age] *
               population->growth->evaluate(0, population->ages[age]);
    }

    numbers_spr[population->n_ages - 1] =
        (numbers_spr[population->n_ages - 2] *
         fims_math::exp(-population->M[population->n_ages - 2])) /
        (1 - fims_math::exp(-population->M[population->n_ages - 1]));
    phi_0 +=
        numbers_spr[population->n_ages - 1] *
        population->proportion_female[population->n_ages - 1] *
        proportion_mature_at_age[population->n_ages - 1] *
        population->growth->evaluate(0,
                                     population->ages[population->n_ages - 1]);

    return phi_0;
  }
};

/**
 * @brief Component for age-structured maturity and recruitment calculations.
 */
template <typename Type>
class AgeStructuredRecruitmentComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const { return "age_structured_recruitment"; }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      size_t year_count = population->n_years + 1;
      RegisterPopulationAgeYearQuantity(
          context, population->GetId(), "proportion_mature_at_age",
          year_count, population->n_ages);
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "expected_recruitment", year_count);
    }
  }

  void CalculateMaturityAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &proportion_mature_at_age =
        GetRequiredDerivedQuantity(population_dq, "proportion_mature_at_age");
    proportion_mature_at_age[i_age_year] =
        population->maturity->evaluate(population->ages[age]);
  }

  void CalculateRecruitment(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t i_dev) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "numbers_at_age");
    fims::Vector<Type> &spawning_biomass =
        GetRequiredDerivedQuantity(population_dq, "spawning_biomass");
    fims::Vector<Type> &expected_recruitment =
        GetRequiredDerivedQuantity(population_dq, "expected_recruitment");
    AgeStructuredBiomassComponent<Type> biomass_component;
    Type phi_0 = biomass_component.CalculateSBPR0(context, population);

    if (i_dev == population->n_years) {
      numbers_at_age[i_age_year] =
          population->recruitment->evaluate_mean(
              spawning_biomass[year - 1], phi_0);
    } else {
      population->recruitment->log_expected_recruitment[year - 1] =
          fims_math::log(population->recruitment->evaluate_mean(
              spawning_biomass[year - 1], phi_0));
      numbers_at_age[i_age_year] = fims_math::exp(
          population->recruitment->process->evaluate_process(year - 1));
    }

    expected_recruitment[year] = numbers_at_age[i_age_year];
  }
};

/**
 * @brief Component for age-structured fleet prediction calculations.
 */
template <typename Type>
class AgeStructuredFleetPredictionComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const {
    return "age_structured_fleet_prediction";
  }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      size_t year_count = population->n_years;
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "total_landings_weight", year_count);
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "total_landings_numbers", year_count);

      for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
        std::shared_ptr<fims_popdy::Fleet<Type>> fleet_ptr =
            population->fleets[fleet];
        RegisterFleetAgeYearQuantity(
            context, fleet_ptr->GetId(), "landings_numbers_at_age",
            population->n_years, population->n_ages);
        RegisterFleetAgeYearQuantity(
            context, fleet_ptr->GetId(), "landings_weight_at_age",
            population->n_years, population->n_ages);
        RegisterFleetAgeYearQuantity(
            context, fleet_ptr->GetId(), "index_numbers_at_age",
            population->n_years, population->n_ages);
        RegisterFleetAgeYearQuantity(
            context, fleet_ptr->GetId(), "index_weight_at_age",
            population->n_years, population->n_ages);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(),
                                  "landings_weight", year_count);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(),
                                  "landings_numbers", year_count);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(), "index_weight",
                                  year_count);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(), "index_numbers",
                                  year_count);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(), "catch_index",
                                  year_count);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(), "expected_catch",
                                  year_count);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(), "expected_index",
                                  year_count);

        if (fleet_ptr->n_lengths > 0) {
          RegisterFleetLengthYearQuantity(
              context, fleet_ptr->GetId(), "landings_numbers_at_length",
              fleet_ptr->n_years, fleet_ptr->n_lengths);
          RegisterFleetLengthYearQuantity(
              context, fleet_ptr->GetId(), "index_numbers_at_length",
              fleet_ptr->n_years, fleet_ptr->n_lengths);
        }
      }
    }
  }

  void CalculateLandings(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year,
      size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &total_landings_weight =
        GetRequiredDerivedQuantity(population_dq, "total_landings_weight");
    fims::Vector<Type> &total_landings_numbers =
        GetRequiredDerivedQuantity(population_dq, "total_landings_numbers");

    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      size_t i_age_year = year * population->n_ages + age;
      fims::Vector<Type> &landings_weight_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "landings_weight_at_age");
      fims::Vector<Type> &landings_weight =
          GetRequiredDerivedQuantity(fleet_dq, "landings_weight");
      fims::Vector<Type> &landings_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers_at_age");
      fims::Vector<Type> &landings_numbers =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers");

      total_landings_weight[year] += landings_weight_at_age[i_age_year];
      landings_weight[year] += landings_weight_at_age[i_age_year];

      total_landings_numbers[year] += landings_numbers_at_age[i_age_year];
      landings_numbers[year] += landings_numbers_at_age[i_age_year];
    }
  }

  void CalculateLandingsWeightAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year,
      size_t age) {
    int i_age_year = year * population->n_ages + age;
    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      fims::Vector<Type> &landings_weight_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "landings_weight_at_age");
      fims::Vector<Type> &landings_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers_at_age");

      landings_weight_at_age[i_age_year] =
          landings_numbers_at_age[i_age_year] *
          population->growth->evaluate(year, population->ages[age]);
    }
  }

  void CalculateLandingsNumbersAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &mortality_Z =
        GetRequiredDerivedQuantity(population_dq, "mortality_Z");
    fims::Vector<Type> &numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "numbers_at_age");

    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      fims::Vector<Type> &landings_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers_at_age");

      landings_numbers_at_age[i_age_year] +=
          (population->fleets[fleet]->Fmort[year] *
           population->f_multiplier[year] *
           population->fleets[fleet]->selectivity->evaluate(
               population->ages[age], year)) /
          mortality_Z[i_age_year] * numbers_at_age[i_age_year] *
          (1 - fims_math::exp(-mortality_Z[i_age_year]));
    }
  }

  void CalculateIndex(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      fims::Vector<Type> &index_weight =
          GetRequiredDerivedQuantity(fleet_dq, "index_weight");
      fims::Vector<Type> &index_weight_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "index_weight_at_age");
      fims::Vector<Type> &index_numbers =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers");
      fims::Vector<Type> &index_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers_at_age");

      index_weight[year] += index_weight_at_age[i_age_year];
      index_numbers[year] += index_numbers_at_age[i_age_year];
    }
  }

  void CalculateIndexNumbersAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population,
      size_t i_age_year, size_t year, size_t age) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &numbers_at_age =
        GetRequiredDerivedQuantity(population_dq, "numbers_at_age");

    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      fims::Vector<Type> &index_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers_at_age");

      index_numbers_at_age[i_age_year] +=
          (population->fleets[fleet]->q.get_force_scalar(year) *
           population->fleets[fleet]->selectivity->evaluate(
               population->ages[age], year)) *
          numbers_at_age[i_age_year];
    }
  }

  void CalculateIndexWeightAA(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year,
      size_t age) {
    int i_age_year = year * population->n_ages + age;
    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      fims::Vector<Type> &index_weight_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "index_weight_at_age");
      fims::Vector<Type> &index_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers_at_age");

      index_weight_at_age[i_age_year] =
          index_numbers_at_age[i_age_year] *
          population->growth->evaluate(year, population->ages[age]);
    }
  }
};

/**
 * @brief Component for age-structured fleet expected observation calculations.
 */
template <typename Type>
class AgeStructuredFleetObservationComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const {
    return "age_structured_fleet_observation";
  }

  virtual void Initialize(ModelContext<Type> &context) {
    for (auto &fleet_entry : context.fleets) {
      std::shared_ptr<fims_popdy::Fleet<Type>> fleet = fleet_entry.second;
      size_t year_count = fleet->n_years;
      RegisterFleetAgeYearQuantity(context, fleet->GetId(), "agecomp_expected",
                                   fleet->n_years, fleet->n_ages);
      RegisterFleetAgeYearQuantity(context, fleet->GetId(),
                                   "agecomp_proportion", fleet->n_years,
                                   fleet->n_ages);
      RegisterFleetYearQuantity(context, fleet->GetId(), "index_expected",
                                year_count);
      RegisterFleetYearQuantity(context, fleet->GetId(), "log_index_expected",
                                year_count);
      RegisterFleetYearQuantity(context, fleet->GetId(), "landings_expected",
                                year_count);
      RegisterFleetYearQuantity(context, fleet->GetId(),
                                "log_landings_expected", year_count);

      if (fleet->n_lengths > 0) {
        RegisterFleetLengthYearQuantity(context, fleet->GetId(),
                                        "lengthcomp_expected", fleet->n_years,
                                        fleet->n_lengths);
        RegisterFleetLengthYearQuantity(context, fleet->GetId(),
                                        "lengthcomp_proportion",
                                        fleet->n_years, fleet->n_lengths);
      }
    }
  }

  void EvaluateAgeComposition(ModelContext<Type> &context) {
    for (auto fleet_iterator = context.fleets.begin();
         fleet_iterator != context.fleets.end(); ++fleet_iterator) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_iterator->second;
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(fleet->GetId());
      fims::Vector<Type> &agecomp_expected =
          GetRequiredDerivedQuantity(fleet_dq, "agecomp_expected");
      fims::Vector<Type> &agecomp_proportion =
          GetRequiredDerivedQuantity(fleet_dq, "agecomp_proportion");
      fims::Vector<Type> &index_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers_at_age");
      fims::Vector<Type> &landings_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers_at_age");

      for (size_t year = 0; year < fleet->n_years; year++) {
        Type sum = static_cast<Type>(0.0);
        Type sum_obs = static_cast<Type>(0.0);

        for (size_t age = 0; age < fleet->n_ages; age++) {
          size_t i_age_year = year * fleet->n_ages + age;
          if (fleet->fleet_observed_landings_data_id_m == -999) {
            agecomp_expected[i_age_year] = index_numbers_at_age[i_age_year];
          } else {
            agecomp_expected[i_age_year] = landings_numbers_at_age[i_age_year];
          }
          sum += agecomp_expected[i_age_year];

          if (fleet->fleet_observed_agecomp_data_id_m != -999) {
            if (fleet->observed_agecomp_data->at(i_age_year) !=
                fleet->observed_agecomp_data->na_value) {
              sum_obs += fleet->observed_agecomp_data->at(i_age_year);
            }
          }
        }

        for (size_t age = 0; age < fleet->n_ages; age++) {
          size_t i_age_year = year * fleet->n_ages + age;
          agecomp_proportion[i_age_year] = agecomp_expected[i_age_year] / sum;

          if (fleet->fleet_observed_agecomp_data_id_m != -999) {
            agecomp_expected[i_age_year] =
                agecomp_proportion[i_age_year] * sum_obs;
          }
        }
      }
    }
  }

  void EvaluateLengthComposition(ModelContext<Type> &context) {
    for (auto fleet_iterator = context.fleets.begin();
         fleet_iterator != context.fleets.end(); ++fleet_iterator) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_iterator->second;
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(fleet->GetId());

      if (fleet->n_lengths == 0) {
        continue;
      }
      fims::Vector<Type> &lengthcomp_expected =
          GetRequiredDerivedQuantity(fleet_dq, "lengthcomp_expected");
      fims::Vector<Type> &lengthcomp_proportion =
          GetRequiredDerivedQuantity(fleet_dq, "lengthcomp_proportion");
      fims::Vector<Type> &agecomp_expected =
          GetRequiredDerivedQuantity(fleet_dq, "agecomp_expected");
      fims::Vector<Type> &landings_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers_at_age");
      fims::Vector<Type> &landings_numbers_at_length =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers_at_length");
      fims::Vector<Type> &index_numbers_at_age =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers_at_age");
      fims::Vector<Type> &index_numbers_at_length =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers_at_length");

      for (size_t year = 0; year < fleet->n_years; year++) {
        Type sum = static_cast<Type>(0.0);
        Type sum_obs = static_cast<Type>(0.0);

        for (size_t length = 0; length < fleet->n_lengths; length++) {
          size_t i_length_year = year * fleet->n_lengths + length;
          for (size_t age = 0; age < fleet->n_ages; age++) {
            size_t i_age_year = year * fleet->n_ages + age;
            size_t i_length_age = age * fleet->n_lengths + length;
            lengthcomp_expected[i_length_year] +=
                agecomp_expected[i_age_year] *
                fleet->age_to_length_conversion[i_length_age];
            landings_numbers_at_length[i_length_year] +=
                landings_numbers_at_age[i_age_year] *
                fleet->age_to_length_conversion[i_length_age];
            index_numbers_at_length[i_length_year] +=
                index_numbers_at_age[i_age_year] *
                fleet->age_to_length_conversion[i_length_age];
          }

          sum += lengthcomp_expected[i_length_year];

          if (fleet->fleet_observed_lengthcomp_data_id_m != -999) {
            if (fleet->observed_lengthcomp_data->at(i_length_year) !=
                fleet->observed_lengthcomp_data->na_value) {
              sum_obs += fleet->observed_lengthcomp_data->at(i_length_year);
            }
          }
        }

        for (size_t length = 0; length < fleet->n_lengths; length++) {
          size_t i_length_year = year * fleet->n_lengths + length;
          lengthcomp_proportion[i_length_year] =
              lengthcomp_expected[i_length_year] / sum;
          if (fleet->fleet_observed_lengthcomp_data_id_m != -999) {
            lengthcomp_expected[i_length_year] =
                lengthcomp_proportion[i_length_year] * sum_obs;
          }
        }
      }
    }
  }

  void EvaluateIndex(ModelContext<Type> &context) {
    for (auto fleet_iterator = context.fleets.begin();
         fleet_iterator != context.fleets.end(); ++fleet_iterator) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_iterator->second;
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(fleet->GetId());
      fims::Vector<Type> &index_numbers =
          GetRequiredDerivedQuantity(fleet_dq, "index_numbers");
      fims::Vector<Type> &index_weight =
          GetRequiredDerivedQuantity(fleet_dq, "index_weight");
      fims::Vector<Type> &index_expected =
          GetRequiredDerivedQuantity(fleet_dq, "index_expected");
      fims::Vector<Type> &log_index_expected =
          GetRequiredDerivedQuantity(fleet_dq, "log_index_expected");

      for (size_t i = 0; i < index_numbers.size(); i++) {
        if (fleet->observed_index_units == "number") {
          index_expected[i] = index_numbers[i];
        } else {
          index_expected[i] = index_weight[i];
        }
        log_index_expected[i] = fims_math::log(index_expected[i]);
      }
    }
  }

  void EvaluateLandings(ModelContext<Type> &context) {
    for (auto fleet_iterator = context.fleets.begin();
         fleet_iterator != context.fleets.end(); ++fleet_iterator) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_iterator->second;
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(fleet->GetId());
      fims::Vector<Type> &landings_numbers =
          GetRequiredDerivedQuantity(fleet_dq, "landings_numbers");
      fims::Vector<Type> &landings_weight =
          GetRequiredDerivedQuantity(fleet_dq, "landings_weight");
      fims::Vector<Type> &landings_expected =
          GetRequiredDerivedQuantity(fleet_dq, "landings_expected");
      fims::Vector<Type> &log_landings_expected =
          GetRequiredDerivedQuantity(fleet_dq, "log_landings_expected");

      for (size_t i = 0; i < landings_weight.size(); i++) {
        if (fleet->observed_landings_units == "number") {
          landings_expected[i] = landings_numbers[i];
        } else {
          landings_expected[i] = landings_weight[i];
        }
        log_landings_expected[i] = fims_math::log(landings_expected[i]);
      }
    }
  }

  virtual void Evaluate(ModelContext<Type> &context) {
    this->EvaluateAgeComposition(context);
    this->EvaluateLengthComposition(context);
    this->EvaluateIndex(context);
    this->EvaluateLandings(context);
  }
};

/**
 * @brief Component that orchestrates age-structured population dynamics.
 */
template <typename Type>
class AgeStructuredDynamicsComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const { return "age_structured_dynamics"; }

  virtual void Initialize(ModelContext<Type> &context) {
    AgeStructuredMortalityComponent<Type> mortality_component;
    AgeStructuredNumbersComponent<Type> numbers_component;
    AgeStructuredBiomassComponent<Type> biomass_component;
    AgeStructuredRecruitmentComponent<Type> recruitment_component;
    AgeStructuredFleetPredictionComponent<Type> fleet_prediction_component;
    AgeStructuredFleetObservationComponent<Type> fleet_observation_component;

    mortality_component.Initialize(context);
    numbers_component.Initialize(context);
    biomass_component.Initialize(context);
    recruitment_component.Initialize(context);
    fleet_prediction_component.Initialize(context);
    fleet_observation_component.Initialize(context);
  }

  virtual void Evaluate(ModelContext<Type> &context) {
    AgeStructuredMortalityComponent<Type> mortality_component;
    AgeStructuredNumbersComponent<Type> numbers_component;
    AgeStructuredBiomassComponent<Type> biomass_component;
    AgeStructuredRecruitmentComponent<Type> recruitment_component;
    AgeStructuredFleetPredictionComponent<Type> fleet_prediction_component;
    AgeStructuredFleetObservationComponent<Type> fleet_observation_component;

    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      std::map<std::string, fims::Vector<Type>> &population_dq =
          context.GetPopulationDerivedQuantities(population->GetId());
      fims::Vector<Type> &expected_recruitment =
          GetRequiredDerivedQuantity(population_dq, "expected_recruitment");
      fims::Vector<Type> &numbers_at_age =
          GetRequiredDerivedQuantity(population_dq, "numbers_at_age");
      fims::Vector<Type> &unfished_numbers_at_age =
          GetRequiredDerivedQuantity(population_dq,
                                     "unfished_numbers_at_age");

      for (size_t year = 0; year <= population->n_years; year++) {
        for (size_t age = 0; age < population->n_ages; age++) {
          size_t i_age_year = year * population->n_ages + age;

          if (year < population->n_years) {
            mortality_component.CalculateMortality(context, population,
                                                   i_age_year, year, age);
          }
          recruitment_component.CalculateMaturityAA(context, population,
                                                    i_age_year, age);

          if (year == 0) {
            numbers_component.CalculateInitialNumbersAA(context, population,
                                                        i_age_year, age);

            if (age == 0) {
              expected_recruitment[year] = numbers_at_age[i_age_year];
              unfished_numbers_at_age[i_age_year] =
                  fims_math::exp(population->recruitment->log_rzero[0]);
            } else {
              numbers_component.CalculateUnfishedNumbersAA(
                  context, population, i_age_year, age - 1, age);
            }
          } else {
            if (age == 0) {
              recruitment_component.CalculateRecruitment(
                  context, population, i_age_year, year, year);
              unfished_numbers_at_age[i_age_year] =
                  fims_math::exp(population->recruitment->log_rzero[0]);
            } else {
              size_t i_agem1_yearm1 =
                  (year - 1) * population->n_ages + (age - 1);
              numbers_component.CalculateNumbersAA(
                  context, population, i_age_year, i_agem1_yearm1, age);
              numbers_component.CalculateUnfishedNumbersAA(
                  context, population, i_age_year, i_agem1_yearm1, age);
            }
          }

          biomass_component.CalculateBiomass(context, population, i_age_year,
                                             year, age);
          biomass_component.CalculateUnfishedBiomass(context, population,
                                                     i_age_year, year, age);
          biomass_component.CalculateSpawningBiomass(context, population,
                                                     i_age_year, year, age);
          biomass_component.CalculateUnfishedSpawningBiomass(
              context, population, i_age_year, year, age);

          if (year < population->n_years) {
            fleet_prediction_component.CalculateLandingsNumbersAA(
                context, population, i_age_year, year, age);
            fleet_prediction_component.CalculateLandingsWeightAA(
                context, population, year, age);
            fleet_prediction_component.CalculateLandings(context, population,
                                                         year, age);

            fleet_prediction_component.CalculateIndexNumbersAA(
                context, population, i_age_year, year, age);
            fleet_prediction_component.CalculateIndexWeightAA(
                context, population, year, age);
            fleet_prediction_component.CalculateIndex(context, population,
                                                      i_age_year, year, age);
          }
        }
        biomass_component.CalculateSpawningBiomassRatio(context, population,
                                                        year);
      }
    }

    fleet_observation_component.Evaluate(context);
  }
};

/**
 * @brief Component that prepares surplus-production depletion transformations.
 */
template <typename Type>
class DepletionPopulationTransformComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const {
    return "depletion_population_transform";
  }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> &population =
          context.populations[p];
      population->n_fleets = population->fleets.size();
    }
  }

  virtual void Prepare(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> &population =
          context.populations[p];
      if (!population->depletion_module) {
        continue;
      }

      population->depletion_module->carrying_capacity[0] =
          fims_math::exp(population->depletion_module->log_carrying_capacity[0]);
      population->depletion_module->growth_rate[0] =
          fims_math::exp(population->depletion_module->log_growth_rate[0]);
      population->depletion_module->shape[0] =
          fims_math::exp(population->depletion_module->log_shape[0]);

      for (size_t i = 0; i < population->depletion_module->depletion.size();
           i++) {
        population->depletion_module->depletion[i] =
            fims_math::exp(population->depletion_module->log_depletion[i]);
      }
    }
  }
};

/**
 * @brief Component for surplus-production depletion dynamics and predictions.
 */
template <typename Type>
class SurplusProductionDynamicsComponent : public ModelComponent<Type> {
 public:
  virtual std::string GetName() const {
    return "surplus_production_dynamics";
  }

  virtual void Initialize(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      if (!population->depletion_module) {
        continue;
      }

      size_t year_count = population->n_years;
      size_t depletion_year_count = population->n_years + 1;
      RegisterPopulationYearQuantity(context, population->GetId(), "biomass",
                                     depletion_year_count);
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "observed_catch", year_count);
      RegisterPopulationYearQuantity(context, population->GetId(),
                                     "harvest_rate", year_count);
      RegisterPopulationScalarQuantity(context, population->GetId(), "fmsy");
      RegisterPopulationScalarQuantity(context, population->GetId(), "bmsy");
      RegisterPopulationScalarQuantity(context, population->GetId(), "msy");

      for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
        std::shared_ptr<fims_popdy::Fleet<Type>> fleet_ptr =
            population->fleets[fleet];
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(),
                                  "index_expected", year_count);
        RegisterFleetYearQuantity(context, fleet_ptr->GetId(),
                                  "log_index_expected", year_count);
        context.RegisterFleetDerivedQuantity(
            fleet_ptr->GetId(),
            "log_index_to_depletion_carrying_capacity_ratio", year_count,
            fims::Vector<int>({static_cast<int>(year_count)}),
            fims::Vector<std::string>({"year"}), static_cast<Type>(-999.0));
        RegisterFleetScalarQuantity(context, fleet_ptr->GetId(), "mean_log_q");
      }
    }
  }

  void CalculateCatch(ModelContext<Type> &context,
                      std::shared_ptr<fims_popdy::Population<Type>> population,
                      size_t year) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &observed_catch =
        GetRequiredDerivedQuantity(population_dq, "observed_catch");

    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      if (population->fleets[fleet]->fleet_observed_landings_data_id_m ==
          -999) {
        continue;
      }
      if (population->fleets[fleet]->observed_landings_data->at(year) !=
          population->fleets[fleet]->observed_landings_data->na_value) {
        observed_catch[year] +=
            population->fleets[fleet]->observed_landings_data->at(year);
      }
    }
  }

  void CalculateDepletion(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &observed_catch =
        GetRequiredDerivedQuantity(population_dq, "observed_catch");

    if (year == 0) {
      population->depletion_module->log_expected_depletion[0] =
          population->depletion_module->log_init_depletion[0];
      return;
    }

    population->depletion_module->log_expected_depletion[year] =
        fims_math::log(fims_math::ad_max(
            population->depletion_module->evaluate_mean(
                population->depletion_module->depletion[year - 1],
                observed_catch[year - 1]),
            static_cast<Type>(0.001)));
  }

  void CalculateIndex(ModelContext<Type> &context,
                      std::shared_ptr<fims_popdy::Population<Type>> population,
                      size_t year) {
    for (auto &fleet_entry : context.fleets) {
      std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fleet_entry.second;
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(fleet->GetId());
      fims::Vector<Type> &log_index_expected =
          GetRequiredDerivedQuantity(fleet_dq, "log_index_expected");
      fims::Vector<Type> &index_expected =
          GetRequiredDerivedQuantity(fleet_dq, "index_expected");

      log_index_expected[year] =
          fims_math::log(population->depletion_module->depletion[year]) +
          fleet->log_q.get_force_scalar(year) +
          fims_math::log(population->depletion_module->carrying_capacity[0]);
      index_expected[year] = fims_math::exp(log_index_expected[year]);
    }
  }

  void CalculateBiomass(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &biomass =
        GetRequiredDerivedQuantity(population_dq, "biomass");
    biomass[year] =
        population->depletion_module->depletion[year] *
        population->depletion_module->carrying_capacity[0];
  }

  void CalculateIndexToDepletionCarryingCapacityRatio(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year) {
    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      fims::Vector<Type> &ratio = GetRequiredDerivedQuantity(
          fleet_dq, "log_index_to_depletion_carrying_capacity_ratio");
      if (population->fleets[fleet]->fleet_observed_index_data_id_m == -999) {
        continue;
      }
      if (population->fleets[fleet]->observed_index_data->at(year) !=
          population->fleets[fleet]->observed_index_data->na_value) {
        ratio[year] =
            fims_math::log(
                population->fleets[fleet]->observed_index_data->at(year)) -
            fims_math::log(population->depletion_module->depletion[year]) -
            fims_math::log(population->depletion_module->carrying_capacity[0]);
      } else {
        ratio[year] = population->fleets[fleet]->observed_index_data->na_value;
      }
    }
  }

  void CalculateMeanLogQ(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population) {
    for (size_t fleet = 0; fleet < population->n_fleets; fleet++) {
      std::map<std::string, fims::Vector<Type>> &fleet_dq =
          context.GetFleetDerivedQuantities(population->fleets[fleet]->GetId());
      fims::Vector<Type> &ratio = GetRequiredDerivedQuantity(
          fleet_dq, "log_index_to_depletion_carrying_capacity_ratio");
      fims::Vector<Type> &mean_log_q =
          GetRequiredDerivedQuantity(fleet_dq, "mean_log_q");
      Type sum_log_q = static_cast<Type>(0.0);
      int n_years = 0;
      for (size_t year = 0; year < population->n_years; year++) {
        if (ratio[year] != -999) {
          sum_log_q += ratio[year];
          n_years++;
        }
      }
      mean_log_q[0] = sum_log_q / n_years;
    }
  }

  void CalculateReferencePoints(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &fmsy =
        GetRequiredDerivedQuantity(population_dq, "fmsy");
    fims::Vector<Type> &bmsy =
        GetRequiredDerivedQuantity(population_dq, "bmsy");
    fims::Vector<Type> &msy =
        GetRequiredDerivedQuantity(population_dq, "msy");

    Type log_fmsy =
        population->depletion_module->log_growth_rate[0] -
        fims_math::log(population->depletion_module->shape[0] - 1) +
        fims_math::log(1.0 - 1 / population->depletion_module->shape[0]);
    fmsy[0] = fims_math::exp(log_fmsy);

    Type log_bmsy =
        population->depletion_module->log_carrying_capacity[0] -
        1 / (population->depletion_module->shape[0] - 1) *
            population->depletion_module->log_shape[0];
    bmsy[0] = fims_math::exp(log_bmsy);
    msy[0] = fmsy[0] * bmsy[0];
  }

  void CalculateHarvestRate(
      ModelContext<Type> &context,
      std::shared_ptr<fims_popdy::Population<Type>> population, size_t year) {
    std::map<std::string, fims::Vector<Type>> &population_dq =
        context.GetPopulationDerivedQuantities(population->GetId());
    fims::Vector<Type> &harvest_rate =
        GetRequiredDerivedQuantity(population_dq, "harvest_rate");
    fims::Vector<Type> &observed_catch =
        GetRequiredDerivedQuantity(population_dq, "observed_catch");
    fims::Vector<Type> &biomass =
        GetRequiredDerivedQuantity(population_dq, "biomass");
    harvest_rate[year] = observed_catch[year] / biomass[year];
  }

  virtual void Evaluate(ModelContext<Type> &context) {
    for (size_t p = 0; p < context.populations.size(); p++) {
      std::shared_ptr<fims_popdy::Population<Type>> population =
          context.populations[p];
      if (!population->depletion_module) {
        continue;
      }

      for (size_t year = 0; year <= population->n_years; year++) {
        if (year < population->n_years) {
          this->CalculateCatch(context, population, year);
        }
        this->CalculateDepletion(context, population, year);
        if (year < population->n_years) {
          this->CalculateIndex(context, population, year);
          this->CalculateIndexToDepletionCarryingCapacityRatio(context,
                                                               population,
                                                               year);
        }
        this->CalculateBiomass(context, population, year);
        if (year < population->n_years) {
          this->CalculateHarvestRate(context, population, year);
        }
      }
      this->CalculateMeanLogQ(context, population);
      this->CalculateReferencePoints(context, population);
    }
  }
};

/**
 * @brief Add a component only when the model does not already contain it.
 */
template <typename Type, typename ComponentType>
void AddComponentIfMissing(FisheryModelBase<Type> &model) {
  if (!model.template HasComponentType<ComponentType>()) {
    model.template EmplaceComponent<ComponentType>();
  }
}

/**
 * @brief Add components shared by fishery model configurations.
 */
template <typename Type>
void AddCommonFisheryComponents(FisheryModelBase<Type> &model) {
  AddComponentIfMissing<Type, ResetDerivedQuantitiesComponent<Type>>(model);
  AddComponentIfMissing<Type, FleetCatchabilityTransformComponent<Type>>(model);
}

/**
 * @brief Add the common surplus-production component stack to a model.
 */
template <typename Type>
void AddSurplusProductionComponents(FisheryModelBase<Type> &model) {
  model.model_type_m = "sp";
  AddCommonFisheryComponents(model);
  AddComponentIfMissing<Type, DepletionPopulationTransformComponent<Type>>(
      model);
  AddComponentIfMissing<Type, SurplusProductionDynamicsComponent<Type>>(model);
}

/**
 * @brief Add shared age-structured preparation components to a model.
 *
 * @details This helper adds only the initialization and transformation pieces
 * shared by age-structured model configurations. Add
 * AgeStructuredDynamicsComponent separately when full catch-at-age dynamics are
 * needed.
 */
template <typename Type>
void AddAgeStructuredPreparationComponents(
    FisheryModelBase<Type> &model) {
  model.model_type_m = "caa";
  AddCommonFisheryComponents(model);
  AddComponentIfMissing<Type, AgeStructuredPopulationTransformComponent<Type>>(
      model);
}

/**
 * @brief Add the catch-at-age component stack to a model.
 */
template <typename Type>
void AddCatchAtAgeComponents(FisheryModelBase<Type> &model) {
  AddAgeStructuredPreparationComponents(model);
  AddComponentIfMissing<Type, AgeStructuredDynamicsComponent<Type>>(model);
}

}  // namespace fims_popdy

#endif
