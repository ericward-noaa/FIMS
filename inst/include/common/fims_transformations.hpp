/**
 * @file fims_transformations.hpp
 * @brief Defines transformations for parameters in FIMS.
 */
#ifndef FIMS_COMMON_FIMS_TRANSFORMATIONS_HPP
#define FIMS_COMMON_FIMS_TRANSFORMATIONS_HPP

#include "def.hpp" 
#include "fims_math.hpp"

namespace fims_transformations {

  template <typename Type>
  inline Type ApplyTransformation(
      const Type& value, 
      const fims::Transformation& transformation) 
{
    switch(label) {
        case fims::Transformation::Label::identity:
            return value;
        case fims::Transformation::Label::exp:
            return exp(value);
        case fims::Transformation::Label::log:
            return log(value);
        case fims::Transformation::Label::logit:
            return fims_math::logit(
            Type(fims::Transformation::Args.lower), 
            Type(fims::Transformation::Args.upper), value);
        case fims::Transformation::Label::square:
            return value * value;
        case fims::Transformation::Label::sqrt:
            return sqrt(value);
        // Add more cases as needed
        default:
            throw std::invalid_argument("Unknown transformation label");
    }
}

  template <typename Type>
  inline Type ApplyBackTransformation(
      const Type& value, 
      const fims::Transformation& transformation) {

    const auto label = transformation.label;
    const auto uncertainty_label = transformation.uncertainty_label;
    const auto args = transformation.args;
    
    Type transformed_value;
    switch (label) {
      case fims::Transformation::Label::identity:
        transformed_value = value;
        break;
      case fims::Transformation::Label::exp:
        transformed_value = fims_math::log(value);
        break;
      case fims::Transformation::Label::log:
        transformed_value = fims_math::exp(value);
        break;
      case fims::Transformation::Label::logit:
        transformed_value = fims_math::inv_logit(Type(args.lower),
          Type(args.upper), value);
        break;
      case fims::Transformation::Label::square:
        transformed_value = fims_math::sqrt(value);
        break;
      case fims::Transformation::Label::sqrt:
        transformed_value = value * value;
        break;
      default:
        throw std::invalid_argument(
        std::string("Unknown transformation applied to a parameter, ") + 
        TransformationLabelToString(label) + 
        std::string(". Valid transformations are identity, exp, log, 
          logit, square, and sqrt."));
        break;
    }
    switch (uncertainty_label) {
      case fims::Transformation::UncertaintyLabel::var:
        return fims_math::sqrt(transformed_value); 
      default:
        return transformed_value; // No uncertainty transformation
    }
  }

  template <typename Type>
  inline Type AddLogJacobian(const fims::Vector<Type>& value, 
      fims::Transformation::Label input_label,
      fims::Transformation::Label prior_label,
      const fims::Transformation::Args& input_args = {},
      const fims::Transformation::Args& prior_args = {})  {

    #ifdef TMB_MODEL
    size_t n = value.size();
    using TMBad::ad_plain;
    
    std::vector<ad_plain> input_value(n);
    std::vector<ad_plain> prior_value(n);

    for(size_t i = 0; i < n; i++) {
      input_value[i] = value[i];
    }

    // Apply input transformation
    std::vector<ad_plain> back_transformed_value(n);
    for (size_t i = 0; i < n; i++)
        transformed_ad[i] = ApplyTransformation(input_ad[i], input_label, input_args);

    // Apply prior transformation
    for (size_t i = 0; i < n; i++)
        prior_ad[i] = ApplyTransformation(transformed_ad[i], prior_label, prior_args);

    // Tape the full input->prior transform:
    auto tape = std::make_unique<TMBad::Tape>();
    tape->Dependent(input_ad, prior_ad);

    // Evaluate the Jacobian at the current value
    Eigen::VectorXd x_val(n);
    for (size_t i = 0; i < n; i++) x_val(i) = static_cast<double>(value[i]);
    Eigen::MatrixXd J = tape->Jacobian(x_val);

    double log_abs_det = TMBad::log_determinant(J);
    return log_abs_det;

    #else
    
    throw std::invalid_argument(
        std::string("Jacobian adjustments currently only work for TMB models.")
    ) 

  }

} // namespace fims_transformations
#endif /* FIMS_COMMON_FIMS_TRANSFORMATIONS_HPP */