#include "gtest/gtest.h"
#include "../../tests/gtest/test_surplus_production_test_fixture.hpp"

namespace
{

    TEST_F(SPEvaluateTestFixture, CalculateBiomass_works)
    {
        std::vector<double> biomass(nyears, 0);
        // calculate biomass in in suplus production module
        for(size_t year_ = 0; year_ < nyears; year_++) {
            this->CalculateCatch(population, year_);
            this->CalculateDepletion(population, year_);
        }

        this->CalculateIndex(population, year);
        this->CalculateBiomass(population, year);

        auto& dq_pop = surplus_production_model->GetPopulationDerivedQuantities(population->GetId());

        biomass[year] = population->depletion_module->depletion[year] * 
                exp(population->depletion_module->log_carrying_capacity[0]);
        EXPECT_EQ( biomass[year], dq_pop["biomass"][year]);

    }
}