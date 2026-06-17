#ifndef FIMS_MODELS_SURPLUS_PRODUCTION_HPP
#define FIMS_MODELS_SURPLUS_PRODUCTION_HPP

#include "composable_fishery_model.hpp"

namespace fims_popdy {

template <typename Type>
/**
 * @brief SurplusProduction is a surplus production fishery model in FIMS.
 *
 */
class SurplusProduction : public ComposableFisheryModel<Type> {
 public:
  /**
   * Constructor for the SurplusProduction class. This constructor initializes the
   * name of the model and sets the id of the model.
   */
  SurplusProduction() : fims_popdy::ComposableFisheryModel<Type>() {
    this->SetModelIdentity("sp", "sp");
    fims_popdy::AddSurplusProductionComponents(*this);
  }

  /**
   * @brief Copy constructor for the SurplusProduction class.
   *
   * @param other The other SurplusProduction object to copy from.
   */
  SurplusProduction(const SurplusProduction &other)
      : ComposableFisheryModel<Type>(other) {
    this->model_type_m = "sp";
  }

  /**
   * @brief Destroy the Surplus Production object.
   *
   */
  virtual ~SurplusProduction() {}

  virtual void Evaluate() {
    this->Prepare();
    fims_popdy::FisheryModelBase<Type>::Evaluate();
  }

  /**
   * @brief Report the results of the surplus-production model.
   */
  virtual void Report() {
    int n_pops = this->populations.size();
    int n_fleets = this->fleets.size();
#ifdef TMB_MODEL
    if (this->do_reporting == true) {
      vector<vector<Type>> log_index_expected_(n_fleets);
      vector<vector<Type>> biomass_(n_pops);
      vector<vector<Type>> observed_catch_(n_pops);
      vector<vector<Type>> depletion_(n_pops);
      vector<vector<Type>> log_depletion_expected_(n_pops);
      vector<vector<Type>> fmsy_(n_pops);
      vector<vector<Type>> bmsy_(n_pops);
      vector<vector<Type>> msy_(n_pops);
      vector<vector<Type>> harvest_rate_(n_pops);
      vector<vector<Type>> log_index_to_depletion_carrying_capacity_ratio_(
          n_fleets);
      vector<vector<Type>> mean_q_(n_fleets);

      int pop_idx = 0;
      for (size_t p = 0; p < this->populations.size(); p++) {
        std::map<std::string, fims::Vector<Type>> &derived_quantities =
            this->GetPopulationDerivedQuantities(
                this->populations[p]->GetId());
        fims::Vector<Type> &log_expected_depletion =
            this->populations[p]->depletion_module->log_expected_depletion;
        biomass_(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "biomass").to_tmb();
        depletion_(pop_idx) =
            this->populations[p]->depletion_module->depletion.to_tmb();
        log_depletion_expected_(pop_idx) = log_expected_depletion.to_tmb();
        observed_catch_(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "observed_catch")
                .to_tmb();
        harvest_rate_(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "harvest_rate")
                .to_tmb();
        fmsy_(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "fmsy").to_tmb();
        bmsy_(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "bmsy").to_tmb();
        msy_(pop_idx) =
            GetRequiredDerivedQuantity(derived_quantities, "msy").to_tmb();

        pop_idx += 1;
      }

      int fleet_idx = 0;
      for (auto fit = this->fleets.begin(); fit != this->fleets.end(); ++fit) {
        std::shared_ptr<fims_popdy::Fleet<Type>> &fleet = fit->second;
        std::map<std::string, fims::Vector<Type>> &derived_quantities =
            this->GetFleetDerivedQuantities(fleet->GetId());
        std::string ratio_name =
            "log_index_to_depletion_carrying_capacity_ratio";
        fims::Vector<Type> &index_to_depletion_ratio =
            GetRequiredDerivedQuantity(derived_quantities, ratio_name);
        log_index_to_depletion_carrying_capacity_ratio_(fleet_idx) =
            index_to_depletion_ratio.to_tmb();
        log_index_expected_(fleet_idx) =
            GetRequiredDerivedQuantity(derived_quantities,
                                       "log_index_expected")
                .to_tmb();
        mean_q_(fleet_idx) =
            fims_math::exp(
                GetRequiredDerivedQuantity(derived_quantities, "mean_log_q")
                    .to_tmb());
        fleet_idx += 1;
      }

      FIMS_REPORT_F_("biomass", biomass_, this->of);
      FIMS_REPORT_F_("depletion", depletion_, this->of);
      FIMS_REPORT_F_("log_depletion_expected", log_depletion_expected_,
                     this->of);
      FIMS_REPORT_F_("observed_catch", observed_catch_, this->of);
      FIMS_REPORT_F_("harvest_rate", harvest_rate_, this->of);
      FIMS_REPORT_F_("fmsy", fmsy_, this->of);
      FIMS_REPORT_F_("bmsy", bmsy_, this->of);
      FIMS_REPORT_F_("msy", msy_, this->of);
      FIMS_REPORT_F_("log_index_expected", log_index_expected_, this->of);
      FIMS_REPORT_F_("mean_q", mean_q_, this->of);
      FIMS_REPORT_F_("log_index_to_depletion_carrying_capacity_ratio",
                     log_index_to_depletion_carrying_capacity_ratio_, this->of);

      vector<Type> biomass = ADREPORTvector(biomass_);
      vector<Type> depletion = ADREPORTvector(depletion_);
      vector<Type> observed_catch = ADREPORTvector(observed_catch_);
      vector<Type> harvest_rate = ADREPORTvector(harvest_rate_);
      vector<Type> fmsy = ADREPORTvector(fmsy_);
      vector<Type> bmsy = ADREPORTvector(bmsy_);
      vector<Type> msy = ADREPORTvector(msy_);
      vector<Type> log_index_expected = ADREPORTvector(log_index_expected_);
      vector<Type> mean_q = ADREPORTvector(mean_q_);
      vector<Type> log_index_to_depletion_carrying_capacity_ratio =
          ADREPORTvector(log_index_to_depletion_carrying_capacity_ratio_);
      ADREPORT_F(biomass, this->of);
      ADREPORT_F(depletion, this->of);
      ADREPORT_F(observed_catch, this->of);
      ADREPORT_F(harvest_rate, this->of);
      ADREPORT_F(fmsy, this->of);
      ADREPORT_F(bmsy, this->of);
      ADREPORT_F(msy, this->of);
      ADREPORT_F(log_index_expected, this->of);
      ADREPORT_F(mean_q, this->of);
      ADREPORT_F(log_index_to_depletion_carrying_capacity_ratio, this->of);
    }
#endif
  }
};

}  // namespace fims_popdy

#endif
