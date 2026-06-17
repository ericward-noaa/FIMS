#ifndef TEST_SURPLUS_PRODUCTION_TEST_FIXTURE_HPP
#define TEST_SURPLUS_PRODUCTION_TEST_FIXTURE_HPP
#include <random>

#include "../../inst/include/models/functors/surplus_production.hpp"
#include "population_dynamics/population/population.hpp"


namespace {

class SPBaseTestFixture : public testing::Test {
 protected: 
 // Common members
  // Use default values from the JABBA example in R testthat example
  // https://github.com/NOAA-FIMS/FIMS/data-raw/data_sp.R
  std::shared_ptr<fims_popdy::Population<double>> population;
  std::shared_ptr<fims_popdy::SurplusProduction<double>> surplus_production_model;
  int id_g = 0;
  int nyears = 70;
  int nfleets = 2;
  int seed = 1234;
  std::default_random_engine generator;

  // Helper function to setup a basic model
  void SetupBasicModel() {
    generator.seed(seed);
    population = std::make_shared<fims_popdy::Population<double>>();
    surplus_production_model = std::make_shared<fims_popdy::SurplusProduction<double>>();
    population->id_g = id_g;
    population->n_years = nyears;
    population->n_fleets = nfleets;

    for (int i = 0; i < nfleets; i++) {
      auto fleet = std::make_shared<fims_popdy::Fleet<double>>();

      fleet->n_years = nyears;
      population->fleets.push_back(fleet);
      surplus_production_model->fleets[fleet->GetId()] =
          fleet;  // Add to SurplusProduction model's fleets map
    }

    // init_depletion
    double depletion_min = 0.01;
    double depletion_max = 0.99;
    std::uniform_real_distribution<double> depletion_distribution(
        depletion_min, depletion_max);
    double init_depletion = depletion_distribution(generator);

    //depletion
    auto depletion = std::make_shared<fims_popdy::PellaTomlinsonDepletion<double>>();
    depletion->log_growth_rate.resize(1);
    depletion->log_carrying_capacity.resize(1);
    depletion->log_shape.resize(1);
    depletion->growth_rate.resize(1);
    depletion->carrying_capacity.resize(1);
    depletion->shape.resize(1);
    depletion->log_growth_rate[0] = fims_math::log(0.2);
    depletion->log_carrying_capacity[0] = fims_math::log(645.0);
    depletion->log_shape[0] = fims_math::log(2.0);
    depletion->log_depletion.resize(nyears);
    for(int i=0; i<depletion->log_depletion.size(); i++){
      double input_depletion = depletion_distribution(generator);
      //log transformation
      depletion->log_depletion[i] = log(input_depletion);
    }
    depletion->depletion.resize(nyears);
    depletion->log_expected_depletion.resize(nyears+1);
    depletion->log_init_depletion.resize(1);
    depletion->log_init_depletion[0] =
          log(init_depletion); //log transformation
    population->depletion_module = depletion;
    surplus_production_model->populations.push_back(population);
  }


  fims_popdy::ModelContext<double> CreateComponentContext() {
    return surplus_production_model->CreateContext();
  }

  void CalculateCatch(
      std::shared_ptr<fims_popdy::Population<double>> &population,
      size_t year) {
    auto context = CreateComponentContext();
    fims_popdy::SurplusProductionDynamicsComponent<double> component;
    component.CalculateCatch(context, population, year);
  }

  void CalculateDepletion(
      std::shared_ptr<fims_popdy::Population<double>> &population,
      size_t year) {
    auto context = CreateComponentContext();
    fims_popdy::SurplusProductionDynamicsComponent<double> component;
    component.CalculateDepletion(context, population, year);
  }

  void CalculateIndex(
      std::shared_ptr<fims_popdy::Population<double>> &population,
      size_t year) {
    auto context = CreateComponentContext();
    fims_popdy::SurplusProductionDynamicsComponent<double> component;
    component.CalculateIndex(context, population, year);
  }

  void CalculateBiomass(
      std::shared_ptr<fims_popdy::Population<double>> &population,
      size_t year) {
    auto context = CreateComponentContext();
    fims_popdy::SurplusProductionDynamicsComponent<double> component;
    component.CalculateBiomass(context, population, year);
  }

  void CalculateReferencePoints(
      std::shared_ptr<fims_popdy::Population<double>> &population) {
    auto context = CreateComponentContext();
    fims_popdy::SurplusProductionDynamicsComponent<double> component;
    component.CalculateReferencePoints(context, population);
  }

  void CalculateHarvestRate(
      std::shared_ptr<fims_popdy::Population<double>> &population,
      size_t year) {
    auto context = CreateComponentContext();
    fims_popdy::SurplusProductionDynamicsComponent<double> component;
    component.CalculateHarvestRate(context, population, year);
  }

};

// Use test fixture to reuse the same configuration of objects for
// several different tests by deriving a class
// from SPBaseTestFixture.
class SPInitializeTestFixture : public SPBaseTestFixture {
     // Make members protected and they can be accessed from
  // sub-classes.
  protected:
  void SetUp() override {
    SetupBasicModel();
    for (auto fit = this->surplus_production_model->fleets.begin();
          fit != this->surplus_production_model->fleets.end(); ++fit) {
      std::shared_ptr<fims_popdy::Fleet<double>> &fleet = (*fit).second;
      surplus_production_model->fleets[fleet->GetId()]->log_q.resize(1);
    }
  }

  // Virtual void TearDown() will be called after each test is
  // run. It needs to be defined if there is clearup work to
  // do. Otherwise, it does not need to be provided.
  virtual void TearDown() {}
 
};


class SPPrepareTestFixture : public SPBaseTestFixture {
  protected:
  void SetUp() override {
    SetupBasicModel();

    // C++ code to set up true values for log_q:

    // log_q
    double log_q_min = fims_math::log(0.1);
    double log_q_max = fims_math::log(1);
    std::uniform_real_distribution<double> log_q_distribution(log_q_min,
                                                              log_q_max);

    // Initialize derived quantities
    surplus_production_model->Initialize();

    // Make a shared pointer to fleet because
    // fleet object needs a shared pointer in fleet.hpp
    // and population object needs a shared pointer in population.hpp
    // (std::vector<std::shared_ptr<fims::Fleet<double> > > fleets;)

    for (auto fit = this->surplus_production_model->fleets.begin();
          fit != this->surplus_production_model->fleets.end(); ++fit) {
      std::shared_ptr<fims_popdy::Fleet<double>> &fleet = (*fit).second;
      surplus_production_model->fleets[fleet->GetId()]->log_q[0] = 
        log_q_distribution(generator);
    }
  }

  virtual void TearDown() {}

};

class SPEvaluateTestFixture : public SPBaseTestFixture {
 protected:

  void SetUp() override {
    SetupBasicModel();
    // C++ code to set up true values for log_naa, log_shape,
    // log_Fmort, and log_q:

    // Setup fleet parameters needed for surplus_production model->Prepare()
    // log_q
    double log_q_min = fims_math::log(0.1);
    double log_q_max = fims_math::log(1);
    std::uniform_real_distribution<double> log_q_distribution(log_q_min,
                                                              log_q_max);
    double catch_min = 9.0;
    double catch_max = 30.0;
    std::uniform_real_distribution<double> catch_distribution(catch_min,
                                                              catch_max);
    // Initialize derived quantities
    surplus_production_model->Initialize();

    // Initialize fleet parameters needed for surplus_production model->Prepare()
    for (auto fit = this->surplus_production_model->fleets.begin();
          fit != this->surplus_production_model->fleets.end(); ++fit) {
      std::shared_ptr<fims_popdy::Fleet<double>> &fleet = (*fit).second;
      auto catch_data = std::make_shared<fims_data_object::DataObject<double>>(nyears);
      for(int year=0; year<nyears; year++){
          catch_data->at(year) = catch_distribution(generator);
      }
      surplus_production_model->fleets[fleet->GetId()]->observed_landings_data = 
        catch_data;
      surplus_production_model->fleets[fleet->GetId()]->fleet_observed_landings_data_id_m = 
        catch_data->id_g;
      surplus_production_model->fleets[fleet->GetId()]->log_q[0] = 
        log_q_distribution(generator);
    }


    // Set initialized values for derived quantities
    surplus_production_model->Prepare();

    int year = 4;
  }

  virtual void TearDown() {}

  int id_g = 0;
  int nyears = 70;
  int nseasons = 1;
  int nfleets = 2;

  int year = 4;
};


}  // namespace
#endif // TEST_SURPLUS_PRODUCTION_TEST_FIXTURE_HPP
