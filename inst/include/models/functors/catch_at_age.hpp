/**
 * @file catch_at_age.hpp
 * @brief Code to specify the catch-at-age model.
 * @copyright This file is part of the NOAA, National Marine Fisheries Service
 * Fisheries Integrated Modeling System project. See LICENSE in the source
 * folder for reuse information.
 */
#ifndef FIMS_MODELS_CATCH_AT_AGE_HPP
#define FIMS_MODELS_CATCH_AT_AGE_HPP

#include "composable_fishery_model.hpp"

namespace fims_popdy {

template <typename Type>
/**
 * @brief CatchAtAge is a class containing a catch-at-age model, which is
 * just one of many potential fishery models that can be used in FIMS. The
 * CatchAtAge class is a composable preset model and can be used to fit both
 * age and length data even though it is called CatchAtAge.
 *
 * See the @ref glossary for definitions of mathematical symbols used below.
 *
 */
class CatchAtAge : public ComposableFisheryModel<Type> {
 public:
  /**
   * Constructor for the CatchAtAge class. This constructor initializes the
   * name of the model and sets the id of the model.
   */
  CatchAtAge() : ComposableFisheryModel<Type>() {
    this->SetModelIdentity("caa", "caa");
    fims_popdy::AddCatchAtAgeComponents(*this);
  }

  /**
   * @brief Copy constructor for the CatchAtAge class.
   *
   * @param other The other CatchAtAge object to copy from.
   */
  CatchAtAge(const CatchAtAge &other)
      : ComposableFisheryModel<Type>(other) {
    this->model_type_m = "caa";
  }

  /**
   * @brief Destroy the Catch At Age object.
   *
   */
  virtual ~CatchAtAge() {}

  virtual void Evaluate() {
    this->Prepare();
    fims_popdy::FisheryModelBase<Type>::Evaluate();
  }
  /**
   * @brief Generate TMB reports from the population dynamics model.
   */
  virtual void Report() {
    int n_fleets = this->fleets.size();
    int n_pops = this->populations.size();
#ifdef TMB_MODEL
    if (this->do_reporting == true) {
      vector<vector<Type>> biomass_p(n_pops);
      vector<vector<Type>> expected_recruitment_p(n_pops);
      vector<vector<Type>> mortality_F_p(n_pops);
      vector<vector<Type>> mortality_M_p(n_pops);
      vector<vector<Type>> mortality_Z_p(n_pops);
      vector<vector<Type>> numbers_at_age_p(n_pops);
      vector<vector<Type>> proportion_mature_at_age_p(n_pops);
      vector<vector<Type>> spawning_biomass_p(n_pops);
      vector<vector<Type>> sum_selectivity_p(n_pops);
      vector<vector<Type>> total_landings_numbers_p(n_pops);
      vector<vector<Type>> total_landings_weight_p(n_pops);
      vector<vector<Type>> unfished_biomass_p(n_pops);
      vector<vector<Type>> unfished_numbers_at_age_p(n_pops);
      vector<vector<Type>> unfished_spawning_biomass_p(n_pops);
      vector<vector<Type>> spawning_biomass_ratio_p(n_pops);

      vector<vector<Type>> agecomp_expected_f(n_fleets);
      vector<vector<Type>> agecomp_proportion_f(n_fleets);
      vector<vector<Type>> catch_index_f(n_fleets);
      vector<vector<Type>> index_expected_f(n_fleets);
      vector<vector<Type>> index_numbers_f(n_fleets);
      vector<vector<Type>> index_numbers_at_age_f(n_fleets);
      vector<vector<Type>> index_numbers_at_length_f(n_fleets);
      vector<vector<Type>> index_weight_f(n_fleets);
      vector<vector<Type>> index_weight_at_age_f(n_fleets);
      vector<vector<Type>> landings_expected_f(n_fleets);
      vector<vector<Type>> landings_numbers_f(n_fleets);
      vector<vector<Type>> landings_numbers_at_age_f(n_fleets);
      vector<vector<Type>> landings_numbers_at_length_f(n_fleets);
      vector<vector<Type>> landings_weight_f(n_fleets);
      vector<vector<Type>> landings_weight_at_age_f(n_fleets);
      vector<vector<Type>> lengthcomp_expected_f(n_fleets);
      vector<vector<Type>> lengthcomp_proportion_f(n_fleets);
      vector<vector<Type>> log_index_expected_f(n_fleets);
      vector<vector<Type>> log_landings_expected_f(n_fleets);

      int pop_idx = 0;
      for (size_t p = 0; p < this->populations.size(); p++) {
        std::map<std::string, fims::Vector<Type>> &derived_quantities =
            this->GetPopulationDerivedQuantities(this->populations[p]->GetId());
        biomass_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "biomass").to_tmb();
        expected_recruitment_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "expected_recruitment")
                .to_tmb();
        mortality_F_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "mortality_F")
                .to_tmb();
        mortality_M_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "mortality_M")
                .to_tmb();
        mortality_Z_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "mortality_Z")
                .to_tmb();
        numbers_at_age_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "numbers_at_age")
                .to_tmb();
        proportion_mature_at_age_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "proportion_mature_at_age")
                .to_tmb();
        spawning_biomass_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "spawning_biomass")
                .to_tmb();
        sum_selectivity_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "sum_selectivity")
                .to_tmb();
        total_landings_numbers_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "total_landings_numbers")
                .to_tmb();
        total_landings_weight_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "total_landings_weight")
                .to_tmb();
        unfished_biomass_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "unfished_biomass")
                .to_tmb();
        unfished_numbers_at_age_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "unfished_numbers_at_age")
                .to_tmb();
        unfished_spawning_biomass_p(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "unfished_spawning_biomass")
                .to_tmb();
        spawning_biomass_ratio_p(pop_idx) =
            this->populations[pop_idx]->spawning_biomass_ratio.to_tmb();

        pop_idx += 1;
      }

      int fleet_idx = 0;
      for (auto fit = this->fleets.begin(); fit != this->fleets.end(); ++fit) {
        std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fit->second;
        std::map<std::string, fims::Vector<Type>> &derived_quantities =
            this->GetFleetDerivedQuantities(fleet->GetId());

        agecomp_expected_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "agecomp_expected")
                .to_tmb();
        agecomp_proportion_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "agecomp_proportion")
                .to_tmb();
        catch_index_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "catch_index")
                .to_tmb();
        index_expected_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "index_expected")
                .to_tmb();
        index_numbers_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "index_numbers")
                .to_tmb();
        index_numbers_at_age_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "index_numbers_at_age")
                .to_tmb();
        auto index_numbers_at_length_it =
            derived_quantities.find("index_numbers_at_length");
        if (index_numbers_at_length_it != derived_quantities.end()) {
          index_numbers_at_length_f(fleet_idx) =
              index_numbers_at_length_it->second.to_tmb();
        }
        index_weight_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "index_weight")
                .to_tmb();
        index_weight_at_age_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "index_weight_at_age")
                .to_tmb();
        landings_expected_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "landings_expected")
                .to_tmb();
        landings_numbers_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "landings_numbers")
                .to_tmb();
        landings_numbers_at_age_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "landings_numbers_at_age")
                .to_tmb();
        auto landings_numbers_at_length_it =
            derived_quantities.find("landings_numbers_at_length");
        if (landings_numbers_at_length_it != derived_quantities.end()) {
          landings_numbers_at_length_f(fleet_idx) =
              landings_numbers_at_length_it->second.to_tmb();
        }
        landings_weight_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "landings_weight")
                .to_tmb();
        landings_weight_at_age_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "landings_weight_at_age")
                .to_tmb();
        auto lengthcomp_expected_it =
            derived_quantities.find("lengthcomp_expected");
        if (lengthcomp_expected_it != derived_quantities.end()) {
          lengthcomp_expected_f(fleet_idx) =
              lengthcomp_expected_it->second.to_tmb();
        }
        auto lengthcomp_proportion_it =
            derived_quantities.find("lengthcomp_proportion");
        if (lengthcomp_proportion_it != derived_quantities.end()) {
          lengthcomp_proportion_f(fleet_idx) =
              lengthcomp_proportion_it->second.to_tmb();
        }
        log_index_expected_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "log_index_expected")
                .to_tmb();
        log_landings_expected_f(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "log_landings_expected")
                .to_tmb();
        fleet_idx += 1;
      }

      vector<Type> biomass = ADREPORTvector(biomass_p);
      vector<Type> expected_recruitment =
          ADREPORTvector(expected_recruitment_p);
      vector<Type> mortality_F = ADREPORTvector(mortality_F_p);
      vector<Type> mortality_M = ADREPORTvector(mortality_M_p);
      vector<Type> mortality_Z = ADREPORTvector(mortality_Z_p);
      vector<Type> numbers_at_age = ADREPORTvector(numbers_at_age_p);
      vector<Type> proportion_mature_at_age =
          ADREPORTvector(proportion_mature_at_age_p);
      vector<Type> spawning_biomass = ADREPORTvector(spawning_biomass_p);
      vector<Type> sum_selectivity = ADREPORTvector(sum_selectivity_p);
      vector<Type> total_landings_numbers =
          ADREPORTvector(total_landings_numbers_p);
      vector<Type> total_landings_weight =
          ADREPORTvector(total_landings_weight_p);
      vector<Type> unfished_biomass = ADREPORTvector(unfished_biomass_p);
      vector<Type> unfished_numbers_at_age =
          ADREPORTvector(unfished_numbers_at_age_p);
      vector<Type> unfished_spawning_biomass =
          ADREPORTvector(unfished_spawning_biomass_p);
      vector<Type> spawning_biomass_ratio =
          ADREPORTvector(spawning_biomass_ratio_p);

      vector<Type> agecomp_expected = ADREPORTvector(agecomp_expected_f);
      vector<Type> agecomp_proportion = ADREPORTvector(agecomp_proportion_f);
      vector<Type> catch_index = ADREPORTvector(catch_index_f);
      vector<Type> index_expected = ADREPORTvector(index_expected_f);
      vector<Type> index_numbers = ADREPORTvector(index_numbers_f);
      vector<Type> index_numbers_at_age =
          ADREPORTvector(index_numbers_at_age_f);
      vector<Type> index_numbers_at_length =
          ADREPORTvector(index_numbers_at_length_f);
      vector<Type> index_weight = ADREPORTvector(index_weight_f);
      vector<Type> index_weight_at_age = ADREPORTvector(index_weight_at_age_f);
      vector<Type> landings_expected = ADREPORTvector(landings_expected_f);
      vector<Type> landings_numbers = ADREPORTvector(landings_numbers_f);
      vector<Type> landings_numbers_at_age =
          ADREPORTvector(landings_numbers_at_age_f);
      vector<Type> landings_numbers_at_length =
          ADREPORTvector(landings_numbers_at_length_f);
      vector<Type> landings_weight = ADREPORTvector(landings_weight_f);
      vector<Type> landings_weight_at_age =
          ADREPORTvector(landings_weight_at_age_f);
      vector<Type> lengthcomp_expected = ADREPORTvector(lengthcomp_expected_f);
      vector<Type> lengthcomp_proportion =
          ADREPORTvector(lengthcomp_proportion_f);
      vector<Type> log_index_expected = ADREPORTvector(log_index_expected_f);
      vector<Type> log_landings_expected =
          ADREPORTvector(log_landings_expected_f);
      FIMS_REPORT_F_("biomass", biomass_p, this->of);
      FIMS_REPORT_F_("expected_recruitment", expected_recruitment_p, this->of);
      FIMS_REPORT_F_("mortality_F", mortality_F_p, this->of);
      FIMS_REPORT_F_("mortality_M", mortality_M_p, this->of);
      FIMS_REPORT_F_("mortality_Z", mortality_Z_p, this->of);
      FIMS_REPORT_F_("numbers_at_age", numbers_at_age_p, this->of);
      FIMS_REPORT_F_("proportion_mature_at_age", proportion_mature_at_age_p,
                     this->of);
      FIMS_REPORT_F_("spawning_biomass", spawning_biomass_p, this->of);
      FIMS_REPORT_F_("sum_selectivity", sum_selectivity_p, this->of);
      FIMS_REPORT_F_("total_landings_numbers", total_landings_numbers_p,
                     this->of);
      FIMS_REPORT_F_("total_landings_weight", total_landings_weight_p,
                     this->of);
      FIMS_REPORT_F_("unfished_biomass", unfished_biomass_p, this->of);
      FIMS_REPORT_F_("unfished_numbers_at_age", unfished_numbers_at_age_p,
                     this->of);
      FIMS_REPORT_F_("unfished_spawning_biomass", unfished_spawning_biomass_p,
                     this->of);
      FIMS_REPORT_F_("spawning_biomass_ratio", spawning_biomass_ratio_p,
                     this->of);

      ADREPORT_F(biomass, this->of);
      ADREPORT_F(expected_recruitment, this->of);
      ADREPORT_F(mortality_F, this->of);
      ADREPORT_F(mortality_M, this->of);
      ADREPORT_F(mortality_Z, this->of);
      ADREPORT_F(numbers_at_age, this->of);
      ADREPORT_F(proportion_mature_at_age, this->of);
      ADREPORT_F(spawning_biomass, this->of);
      ADREPORT_F(sum_selectivity, this->of);
      ADREPORT_F(total_landings_numbers, this->of);
      ADREPORT_F(total_landings_weight, this->of);
      ADREPORT_F(unfished_biomass, this->of);
      ADREPORT_F(unfished_numbers_at_age, this->of);
      ADREPORT_F(unfished_spawning_biomass, this->of);
      ADREPORT_F(spawning_biomass_ratio, this->of);

      FIMS_REPORT_F_("agecomp_expected", agecomp_expected_f, this->of);
      FIMS_REPORT_F_("agecomp_proportion", agecomp_proportion_f, this->of);
      FIMS_REPORT_F_("catch_index", catch_index_f, this->of);
      FIMS_REPORT_F_("index_expected", index_expected_f, this->of);
      FIMS_REPORT_F_("index_numbers", index_numbers_f, this->of);
      FIMS_REPORT_F_("index_numbers_at_age", index_numbers_at_age_f, this->of);
      FIMS_REPORT_F_("index_numbers_at_length", index_numbers_at_length_f,
                     this->of);
      FIMS_REPORT_F_("index_weight", index_weight_f, this->of);
      FIMS_REPORT_F_("index_weight_at_age", index_weight_at_age_f, this->of);
      FIMS_REPORT_F_("landings_expected", landings_expected_f, this->of);
      FIMS_REPORT_F_("landings_numbers", landings_numbers_f, this->of);
      FIMS_REPORT_F_("landings_numbers_at_age", landings_numbers_at_age_f,
                     this->of);
      FIMS_REPORT_F_("landings_numbers_at_length", landings_numbers_at_length_f,
                     this->of);
      FIMS_REPORT_F_("landings_weight", landings_weight_f, this->of);
      FIMS_REPORT_F_("landings_weight_at_age", landings_weight_at_age_f,
                     this->of);
      FIMS_REPORT_F_("lengthcomp_expected", lengthcomp_expected_f, this->of);
      FIMS_REPORT_F_("lengthcomp_proportion", lengthcomp_proportion_f,
                     this->of);
      FIMS_REPORT_F_("log_index_expected", log_index_expected_f, this->of);
      FIMS_REPORT_F_("log_landings_expected", log_landings_expected_f,
                     this->of);
      ADREPORT_F(agecomp_expected, this->of);
      ADREPORT_F(agecomp_proportion, this->of);
      ADREPORT_F(catch_index, this->of);
      ADREPORT_F(index_expected, this->of);
      ADREPORT_F(index_numbers, this->of);
      ADREPORT_F(index_numbers_at_age, this->of);
      ADREPORT_F(index_numbers_at_length, this->of);
      ADREPORT_F(index_weight, this->of);
      ADREPORT_F(index_weight_at_age, this->of);
      ADREPORT_F(landings_expected, this->of);
      ADREPORT_F(landings_numbers, this->of);
      ADREPORT_F(landings_numbers_at_age, this->of);
      ADREPORT_F(landings_numbers_at_length, this->of);
      ADREPORT_F(landings_weight, this->of);
      ADREPORT_F(landings_weight_at_age, this->of);
      ADREPORT_F(lengthcomp_expected, this->of);
      ADREPORT_F(lengthcomp_proportion, this->of);
      ADREPORT_F(log_index_expected, this->of);
      ADREPORT_F(log_landings_expected, this->of);
    }
#endif
  }
};

}  // namespace fims_popdy

#endif
