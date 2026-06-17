/**
 * @file fishery_model_base.hpp
 * @brief Defines the base class for all fishery models within the FIMS
 * framework.
 * @copyright This file is part of the NOAA, National Marine Fisheries Service
 * Fisheries Integrated Modeling System project. See LICENSE in the source
 * folder for reuse information.
 */
#ifndef FIMS_MODELS_FISHERY_MODEL_BASE_HPP
#define FIMS_MODELS_FISHERY_MODEL_BASE_HPP

#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "../../common/model_object.hpp"
#include "../../common/fims_math.hpp"
#include "../../common/fims_vector.hpp"
#include "../../population_dynamics/population/population.hpp"
/**
 * @brief The population dynamics of FIMS.
 *
 */
namespace fims_popdy {

/**
 * @brief Structure to hold dimension information for derived quantities.
 */
struct DimensionInfo {
  std::string name;                    /*!< name of the derived quantity */
  int ndims;                           /*!< number of dimensions */
  fims::Vector<int> dims;              /*!< vector of dimensions */
  fims::Vector<std::string> dim_names; /*!< vector of dimension names */
  fims::Vector<double> se_values_m;    /*!< final values of the report vector */

  /**
   * @brief Default constructor for dimension information.
   */
  DimensionInfo() : ndims(0) {}

  /**
   * @brief Constructor with parameters.
   * @param name The name of the derived quantity.
   * @param dims A vector of integers representing the dimensions.
   * @param dim_names A vector of strings representing the names of the
   * dimensions.
   */
  DimensionInfo(const std::string &name, const fims::Vector<int> &dims,
                const fims::Vector<std::string> &dim_names)
      : name(name), ndims(dims.size()), dims(dims), dim_names(dim_names) {}

  /**
   * Copy constructor
   */
  DimensionInfo(const DimensionInfo &other)
      : name(other.name),
        ndims(other.dims.size()),
        dims(other.dims),
        dim_names(other.dim_names) {}

  /**
   * @brief Assignment operator for DimensionInfo.
   */
  DimensionInfo &operator=(const DimensionInfo &other) {
    if (this != &other) {
      name = other.name;
      ndims = other.ndims;
      dims = other.dims;
      dim_names = other.dim_names;
      se_values_m = other.se_values_m;
    }
    return *this;
  }
};

/**
 * @brief Register dimension metadata for a derived quantity.
 */
inline void RegisterDimensionInfo(
    std::map<std::string, DimensionInfo> &dimension_info,
    const std::string &name, const fims::Vector<int> &dims,
    const fims::Vector<std::string> &dim_names) {
  dimension_info[name] = DimensionInfo(name, dims, dim_names);
}

/**
 * @brief Resize or create a derived quantity while preserving report tags.
 */
template <typename Type>
fims::Vector<Type> &RegisterDerivedQuantity(
    std::map<std::string, fims::Vector<Type>> &derived_quantities,
    const std::string &name, size_t size,
    Type value = static_cast<Type>(0.0)) {
  auto it = derived_quantities.find(name);
  if (it == derived_quantities.end() || it->second.size() != size) {
    std::string tag =
        (it == derived_quantities.end()) ? "" : it->second.get_tag();
    derived_quantities[name] = fims::Vector<Type>(size, value);
    derived_quantities[name].set_tag(tag);
  }
  return derived_quantities[name];
}

/**
 * @brief Get a derived quantity without creating report-time placeholders.
 */
template <typename Type>
fims::Vector<Type> &GetRequiredDerivedQuantity(
    std::map<std::string, fims::Vector<Type>> &derived_quantities,
    const std::string &name) {
  auto it = derived_quantities.find(name);
  if (it == derived_quantities.end()) {
    throw std::out_of_range("Required derived quantity '" + name +
                            "' has not been registered");
  }
  return it->second;
}

/**
 * @brief Shared state passed to model components during model execution.
 *
 * @details ModelContext intentionally holds references to the model-owned
 * population, fleet, derived quantity, and dimension containers. Components can
 * therefore be composed without becoming top-level model families themselves.
 */
template <typename Type>
struct ModelContext {
  typedef typename std::map<uint32_t, std::map<std::string, fims::Vector<Type>>>
      DerivedQuantitiesMap;
  typedef typename std::map<uint32_t, std::map<std::string, DimensionInfo>>
      DimensionInfoMap;

  std::vector<std::shared_ptr<fims_popdy::Population<Type>>> &populations;
  std::map<uint32_t, std::shared_ptr<fims_popdy::Fleet<Type>>> &fleets;
  DerivedQuantitiesMap &fleet_derived_quantities;
  DerivedQuantitiesMap &population_derived_quantities;
  DimensionInfoMap &fleet_dimension_info;
  DimensionInfoMap &population_dimension_info;

  ModelContext(
      std::vector<std::shared_ptr<fims_popdy::Population<Type>>> &populations,
      std::map<uint32_t, std::shared_ptr<fims_popdy::Fleet<Type>>> &fleets,
      DerivedQuantitiesMap &fleet_derived_quantities,
      DerivedQuantitiesMap &population_derived_quantities,
      DimensionInfoMap &fleet_dimension_info,
      DimensionInfoMap &population_dimension_info)
      : populations(populations),
        fleets(fleets),
        fleet_derived_quantities(fleet_derived_quantities),
        population_derived_quantities(population_derived_quantities),
        fleet_dimension_info(fleet_dimension_info),
        population_dimension_info(population_dimension_info) {}

  std::map<std::string, fims::Vector<Type>> &GetFleetDerivedQuantities(
      uint32_t fleet_id) {
    auto it = fleet_derived_quantities.find(fleet_id);
    if (it == fleet_derived_quantities.end()) {
      std::stringstream ss;
      ss << "ModelContext: fleet_id " << fleet_id
         << " not found in fleet_derived_quantities";
      throw std::out_of_range(ss.str());
    }
    return it->second;
  }

  std::map<std::string, fims::Vector<Type>> &EnsureFleetDerivedQuantities(
      uint32_t fleet_id) {
    return fleet_derived_quantities[fleet_id];
  }

  std::map<std::string, DimensionInfo> &EnsureFleetDimensionInfo(
      uint32_t fleet_id) {
    return fleet_dimension_info[fleet_id];
  }

  void RegisterFleetDimensionInfo(
      uint32_t fleet_id, const std::string &name,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names) {
    fims_popdy::RegisterDimensionInfo(this->EnsureFleetDimensionInfo(fleet_id),
                                      name, dims, dim_names);
  }

  fims::Vector<Type> &RegisterFleetDerivedQuantity(
      uint32_t fleet_id, const std::string &name, size_t size,
      Type value = static_cast<Type>(0.0)) {
    return fims_popdy::RegisterDerivedQuantity(
        this->EnsureFleetDerivedQuantities(fleet_id), name, size, value);
  }

  fims::Vector<Type> &RegisterFleetDerivedQuantity(
      uint32_t fleet_id, const std::string &name, size_t size,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names,
      Type value = static_cast<Type>(0.0)) {
    fims::Vector<Type> &quantity =
        this->RegisterFleetDerivedQuantity(fleet_id, name, size, value);
    this->RegisterFleetDimensionInfo(fleet_id, name, dims, dim_names);
    return quantity;
  }

  std::map<std::string, fims::Vector<Type>> &GetPopulationDerivedQuantities(
      uint32_t population_id) {
    auto it = population_derived_quantities.find(population_id);
    if (it == population_derived_quantities.end()) {
      std::stringstream ss;
      ss << "ModelContext: population_id " << population_id
         << " not found in population_derived_quantities";
      throw std::out_of_range(ss.str());
    }
    return it->second;
  }

  std::map<std::string, fims::Vector<Type>> &EnsurePopulationDerivedQuantities(
      uint32_t population_id) {
    return population_derived_quantities[population_id];
  }

  std::map<std::string, DimensionInfo> &EnsurePopulationDimensionInfo(
      uint32_t population_id) {
    return population_dimension_info[population_id];
  }

  void RegisterPopulationDimensionInfo(
      uint32_t population_id, const std::string &name,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names) {
    fims_popdy::RegisterDimensionInfo(
        this->EnsurePopulationDimensionInfo(population_id), name, dims,
        dim_names);
  }

  fims::Vector<Type> &RegisterPopulationDerivedQuantity(
      uint32_t population_id, const std::string &name, size_t size,
      Type value = static_cast<Type>(0.0)) {
    return fims_popdy::RegisterDerivedQuantity(
        this->EnsurePopulationDerivedQuantities(population_id), name, size,
        value);
  }

  fims::Vector<Type> &RegisterPopulationDerivedQuantity(
      uint32_t population_id, const std::string &name, size_t size,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names,
      Type value = static_cast<Type>(0.0)) {
    fims::Vector<Type> &quantity =
        this->RegisterPopulationDerivedQuantity(population_id, name, size,
                                                value);
    this->RegisterPopulationDimensionInfo(population_id, name, dims, dim_names);
    return quantity;
  }
};

/**
 * @brief A composable unit of fishery model behavior.
 *
 * @details Components are intentionally smaller than historical model-family
 * classes. A component can own one process, transformation, prediction, or
 * reporting responsibility and can be assembled with other components.
 */
template <typename Type>
class ModelComponent {
 public:
  virtual ~ModelComponent() {}
  virtual std::string GetName() const { return "ModelComponent"; }
  virtual void Initialize(ModelContext<Type> &context) {}
  virtual void Prepare(ModelContext<Type> &context) {}
  virtual void Evaluate(ModelContext<Type> &context) {}
  virtual void Report(ModelContext<Type> &context) {}
};

/**
 * @brief Lifecycle stage used when running model components.
 */
enum class ModelComponentStage {
  kInitialize,
  kPrepare,
  kEvaluate,
  kReport
};

/**
 * @brief FisheryModelBase is a base class for fishery models in FIMS.
 *
 */
template <typename Type>
class FisheryModelBase : public fims_model_object::FIMSObject<Type> {
  static uint32_t id_g; /*!< global id where unique id is drawn from for fishery
                           model object*/
  uint32_t id; /*!< unique identifier assigned for all fishery model objects */

 public:
#ifdef TMB_MODEL
  bool do_reporting =
      true; /*!< flag to control reporting of derived quantities */
#endif
  /**
   * @brief A string specifying the model type.
   *
   */
  std::string name_m;
  std::string model_type_m;
  /**
   * @brief Unique identifier for the fishery model.
   *
   */
  std::set<uint32_t> population_ids;
  /**
   * @brief A vector of populations in the fishery model.
   *
   */
  std::vector<std::shared_ptr<fims_popdy::Population<Type>>> populations;
  /**
   * @brief A map of fleets in the fishery model, indexed by fleet id.
   * Unique instances to eliminate duplicate initialization.
   *
   */
  std::map<uint32_t, std::shared_ptr<fims_popdy::Fleet<Type>>> fleets;
  /**
   * @brief Fleet-based iterator.
   *
   */
  typedef typename std::map<uint32_t,
                            std::shared_ptr<fims_popdy::Fleet<Type>>>::iterator
      fleet_iterator;

  /**
   * @brief Type definitions for derived quantities and dimension information
   * maps.
   */
  typedef typename std::map<uint32_t, std::map<std::string, fims::Vector<Type>>>
      DerivedQuantitiesMap;

  /**
   * @brief Iterator for the derived quantities map.
   */
  typedef typename DerivedQuantitiesMap::iterator DerivedQuantitiesMapIterator;

  /**
   * @brief Shared pointer for the fleet derived quantities map.
   */
  std::shared_ptr<DerivedQuantitiesMap> fleet_derived_quantities_ptr;

  /**
   * @brief Shared pointer for the population derived quantities map.
   */
  std::shared_ptr<DerivedQuantitiesMap> population_derived_quantities_ptr;

  /**
   * @brief Type definitions for dimension information maps.
   */
  typedef typename std::map<uint32_t, std::map<std::string, DimensionInfo>>
      DimensionInfoMap;

  /**
   * @brief Shared pointer for the fleet dimension information map.
   */
  std::shared_ptr<DimensionInfoMap> fleet_dimension_info;

  /**
   * @brief Shared pointer for the population dimension information map.
   */
  std::shared_ptr<DimensionInfoMap> population_dimension_info;

  /**
   * @brief Ordered components used by composable fishery models.
   */
  std::vector<std::shared_ptr<ModelComponent<Type>>> components;

#ifdef TMB_MODEL
  ::objective_function<Type> *of;
#endif
  /**
   * @brief Construct a new Fishery Model Base object.
   *
   */
  FisheryModelBase() : id(FisheryModelBase::id_g++) {
    fleet_derived_quantities_ptr = std::make_shared<DerivedQuantitiesMap>();
    population_derived_quantities_ptr = std::make_shared<DerivedQuantitiesMap>();
    fleet_dimension_info = std::make_shared<DimensionInfoMap>();
    population_dimension_info = std::make_shared<DimensionInfoMap>();
  }

  /**
   * @brief Construct a new Fishery Model Base object.
   *
   * @param other
   */
  FisheryModelBase(const FisheryModelBase &other)
        : id(other.id),      
        name_m(other.name_m),
        model_type_m(other.model_type_m),
        population_ids(other.population_ids),
        populations(other.populations),
        fleets(other.fleets),
        fleet_derived_quantities_ptr(other.fleet_derived_quantities_ptr),
        population_derived_quantities_ptr(other.population_derived_quantities_ptr),
        fleet_dimension_info(other.fleet_dimension_info),
        population_dimension_info(other.population_dimension_info),
        components(other.components) {}

  /**
   * @brief Destroy the Fishery Model Base object.
   *
   */
  virtual ~FisheryModelBase() {}

  /**
   * @brief Get the fleet dimension information.
   *
   * @return std::map<uint32_t, std::map<std::string, DimensionInfo>>
   */
  std::map<uint32_t, std::map<std::string, DimensionInfo>> &
  GetFleetDimensionInfo() {
    return *fleet_dimension_info;
  }

  /**
   * @brief Get the population dimension information.
   *
   * @return std::map<uint32_t, std::map<std::string, DimensionInfo>>
   */
  std::map<uint32_t, std::map<std::string, DimensionInfo>> &
  GetPopulationDimensionInfo() {
    return *population_dimension_info;
  }

  /**
   * @brief Get the fleet derived quantities.
   *
   * @return DerivedQuantitiesMap
   */
  DerivedQuantitiesMap &GetFleetDerivedQuantities() {
    return *fleet_derived_quantities_ptr;
  }

  /**
   * @brief Get the population derived quantities.
   *
   * @return DerivedQuantitiesMap
   */
  DerivedQuantitiesMap &GetPopulationDerivedQuantities() {
    return *population_derived_quantities_ptr;
  }

  /**
   * @brief Get the fleet derived quantities for a specified fleet.
   *
   * @param fleet_id The ID of the fleet.
   * @return std::map<std::string, fims::Vector<Type>>&
   */
  std::map<std::string, fims::Vector<Type>> &GetFleetDerivedQuantities(
      uint32_t fleet_id) {
    if (!fleet_derived_quantities_ptr) {
      throw std::runtime_error(
          "GetFleetDerivedQuantities: fleet_derived_quantities_ptr is null");
    }
    auto &outer = *fleet_derived_quantities_ptr;
    auto it = outer.find(fleet_id);
    if (it == outer.end()) {
      std::stringstream ss;

      ss << "GetFleetDerivedQuantities: fleet_id " << fleet_id
         << " not found in fleet_derived_quantities_ptr";
      throw std::out_of_range(ss.str());
    }
    return it->second;
  }

  std::map<std::string, fims::Vector<Type>> &EnsureFleetDerivedQuantities(
      uint32_t fleet_id) {
    if (!fleet_derived_quantities_ptr) {
      fleet_derived_quantities_ptr = std::make_shared<
          std::map<uint32_t, std::map<std::string, fims::Vector<Type>>>>();
    }

    auto &outer = *fleet_derived_quantities_ptr;
    return outer[fleet_id];
  }

  std::map<std::string, DimensionInfo> &EnsureFleetDimensionInfo(
      uint32_t fleet_id) {
    return (*fleet_dimension_info)[fleet_id];
  }

  void RegisterFleetDimensionInfo(
      uint32_t fleet_id, const std::string &name,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names) {
    fims_popdy::RegisterDimensionInfo(this->EnsureFleetDimensionInfo(fleet_id),
                                      name, dims, dim_names);
  }

  fims::Vector<Type> &RegisterFleetDerivedQuantity(
      uint32_t fleet_id, const std::string &name, size_t size,
      Type value = static_cast<Type>(0.0)) {
    return fims_popdy::RegisterDerivedQuantity(
        this->EnsureFleetDerivedQuantities(fleet_id), name, size, value);
  }

  fims::Vector<Type> &RegisterFleetDerivedQuantity(
      uint32_t fleet_id, const std::string &name, size_t size,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names,
      Type value = static_cast<Type>(0.0)) {
    fims::Vector<Type> &quantity =
        this->RegisterFleetDerivedQuantity(fleet_id, name, size, value);
    this->RegisterFleetDimensionInfo(fleet_id, name, dims, dim_names);
    return quantity;
  }

  std::map<std::string, fims::Vector<Type>> &EnsurePopulationDerivedQuantities(
      uint32_t population_id) {
    if (!population_derived_quantities_ptr) {
      population_derived_quantities_ptr = std::make_shared<
          std::map<uint32_t, std::map<std::string, fims::Vector<Type>>>>();
    }

    auto &outer = *population_derived_quantities_ptr;
    return outer[population_id];
  }

  std::map<std::string, DimensionInfo> &EnsurePopulationDimensionInfo(
      uint32_t population_id) {
    return (*population_dimension_info)[population_id];
  }

  void RegisterPopulationDimensionInfo(
      uint32_t population_id, const std::string &name,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names) {
    fims_popdy::RegisterDimensionInfo(
        this->EnsurePopulationDimensionInfo(population_id), name, dims,
        dim_names);
  }

  fims::Vector<Type> &RegisterPopulationDerivedQuantity(
      uint32_t population_id, const std::string &name, size_t size,
      Type value = static_cast<Type>(0.0)) {
    return fims_popdy::RegisterDerivedQuantity(
        this->EnsurePopulationDerivedQuantities(population_id), name, size,
        value);
  }

  fims::Vector<Type> &RegisterPopulationDerivedQuantity(
      uint32_t population_id, const std::string &name, size_t size,
      const fims::Vector<int> &dims,
      const fims::Vector<std::string> &dim_names,
      Type value = static_cast<Type>(0.0)) {
    fims::Vector<Type> &quantity =
        this->RegisterPopulationDerivedQuantity(population_id, name, size,
                                                value);
    this->RegisterPopulationDimensionInfo(population_id, name, dims, dim_names);
    return quantity;
  }

  /**
   * @brief Get the population derived quantities for a specified population.
   *
   * @param population_id The ID of the population.
   * @return std::map<std::string, fims::Vector<Type>>&
   */
  std::map<std::string, fims::Vector<Type>> &GetPopulationDerivedQuantities(
      uint32_t population_id) {
    if (!population_derived_quantities_ptr) {
      throw std::runtime_error(
          "GetPopulationDerivedQuantities: population_derived_quantities_ptr is "
          "null");
    }
    auto &outer = *population_derived_quantities_ptr;
    auto it = outer.find(population_id);
    if (it == outer.end()) {
      std::ostringstream ss;
      ss << "GetPopulationDerivedQuantities: population_id " << population_id
         << " not found in population_derived_quantities_ptr";
      throw std::out_of_range(ss.str());
    }
    return it->second;
  }

  /**
   * @brief Get the fleet dimension information for a specified fleet.
   *
   * @param fleet_id The ID of the fleet.
   * @return std::map<std::string, DimensionInfo>
   */
  std::map<std::string, DimensionInfo> &GetFleetDimensionInfo(
      uint32_t fleet_id) {
    return (*fleet_dimension_info)[fleet_id];
  }

  /**
   * @brief Get the population dimension information for a specified population.
   *
   * @param population_id The ID of the population.
   * @return std::map<std::string, DimensionInfo>
   */
  std::map<std::string, DimensionInfo> &GetPopulationDimensionInfo(
      uint32_t population_id) {
    return (*population_dimension_info)[population_id];
  }

  /**
   * @brief Set model type and generated name from a prefix.
   */
  void SetModelIdentity(const std::string &model_type,
                        const std::string &name_prefix) {
    std::stringstream ss;
    ss << name_prefix << "_" << this->id << "_";
    this->name_m = ss.str();
    this->model_type_m = model_type;
  }

  /**
   * @brief Add a population id to the model.
   */
  void AddPopulation(uint32_t id) { this->population_ids.insert(id); }

  /**
   * @brief Get the population ids linked to this model.
   */
  std::set<uint32_t> &GetPopulationIds() { return this->population_ids; }

  /**
   * @brief Get the population ids linked to this model.
   */
  const std::set<uint32_t> &GetPopulationIds() const {
    return this->population_ids;
  }

  /**
   * @brief Get the populations linked to this model.
   */
  std::vector<std::shared_ptr<fims_popdy::Population<Type>>> &GetPopulations() {
    return this->populations;
  }

  /**
   * @brief Get the populations linked to this model.
   */
  const std::vector<std::shared_ptr<fims_popdy::Population<Type>>>
      &GetPopulations() const {
    return this->populations;
  }

  /**
   * @brief Create a context object that gives components access to model state.
   */
  ModelContext<Type> CreateContext() {
    return ModelContext<Type>(
        this->populations, this->fleets, *this->fleet_derived_quantities_ptr,
        *this->population_derived_quantities_ptr, *this->fleet_dimension_info,
        *this->population_dimension_info);
  }

  /**
   * @brief Add a component to the model evaluation pipeline.
   *
   * @param component Shared pointer to the component to add.
   */
  void AddComponent(std::shared_ptr<ModelComponent<Type>> component) {
    if (!component) {
      throw std::invalid_argument("AddComponent: component is null");
    }
    this->components.push_back(component);
  }

  /**
   * @brief Insert a component at a specific pipeline position.
   *
   * @param index Position where the component should be inserted.
   * @param component Shared pointer to the component to insert.
   */
  void InsertComponent(size_t index,
                       std::shared_ptr<ModelComponent<Type>> component) {
    if (!component) {
      throw std::invalid_argument("InsertComponent: component is null");
    }
    if (index > this->components.size()) {
      throw std::out_of_range("InsertComponent: index is out of range");
    }
    this->components.insert(this->components.begin() + index, component);
  }

  /**
   * @brief Replace a component at a specific pipeline position.
   *
   * @param index Position of the component to replace.
   * @param component Shared pointer to the replacement component.
   */
  void ReplaceComponent(size_t index,
                        std::shared_ptr<ModelComponent<Type>> component) {
    if (!component) {
      throw std::invalid_argument("ReplaceComponent: component is null");
    }
    if (index >= this->components.size()) {
      throw std::out_of_range("ReplaceComponent: index is out of range");
    }
    this->components[index] = component;
  }

  /**
   * @brief Construct and add a component to the model evaluation pipeline.
   *
   * @return Shared pointer to the newly added component.
   */
  template <typename ComponentType, typename... Args>
  std::shared_ptr<ComponentType> EmplaceComponent(Args &&...args) {
    static_assert(std::is_base_of<ModelComponent<Type>, ComponentType>::value,
                  "ComponentType must inherit from ModelComponent<Type>");
    std::shared_ptr<ComponentType> component =
        std::make_shared<ComponentType>(std::forward<Args>(args)...);
    this->AddComponent(component);
    return component;
  }

  /**
   * @brief Construct and insert a component at a specific pipeline position.
   *
   * @return Shared pointer to the newly inserted component.
   */
  template <typename ComponentType, typename... Args>
  std::shared_ptr<ComponentType> EmplaceComponentAt(size_t index,
                                                   Args &&...args) {
    static_assert(std::is_base_of<ModelComponent<Type>, ComponentType>::value,
                  "ComponentType must inherit from ModelComponent<Type>");
    std::shared_ptr<ComponentType> component =
        std::make_shared<ComponentType>(std::forward<Args>(args)...);
    this->InsertComponent(index, component);
    return component;
  }

  /**
   * @brief Construct and replace a component at a specific pipeline position.
   *
   * @return Shared pointer to the newly inserted component.
   */
  template <typename ComponentType, typename... Args>
  std::shared_ptr<ComponentType> EmplaceComponentReplacement(size_t index,
                                                            Args &&...args) {
    static_assert(std::is_base_of<ModelComponent<Type>, ComponentType>::value,
                  "ComponentType must inherit from ModelComponent<Type>");
    std::shared_ptr<ComponentType> component =
        std::make_shared<ComponentType>(std::forward<Args>(args)...);
    this->ReplaceComponent(index, component);
    return component;
  }

  /**
   * @brief Get the ordered list of model components.
   */
  std::vector<std::shared_ptr<ModelComponent<Type>>> &GetComponents() {
    return this->components;
  }

  /**
   * @brief Get the ordered list of model components.
   */
  const std::vector<std::shared_ptr<ModelComponent<Type>>> &GetComponents()
      const {
    return this->components;
  }

  /**
   * @brief Remove all model components from the evaluation pipeline.
   */
  void ClearComponents() { this->components.clear(); }

  /**
   * @brief Find the first component with the requested name.
   */
  std::shared_ptr<ModelComponent<Type>> FindComponent(
      const std::string &component_name) {
    for (size_t i = 0; i < this->components.size(); i++) {
      if (this->components[i]->GetName() == component_name) {
        return this->components[i];
      }
    }
    return std::shared_ptr<ModelComponent<Type>>();
  }

  /**
   * @brief Find the first component with the requested name.
   */
  std::shared_ptr<const ModelComponent<Type>> FindComponent(
      const std::string &component_name) const {
    for (size_t i = 0; i < this->components.size(); i++) {
      if (this->components[i]->GetName() == component_name) {
        return this->components[i];
      }
    }
    return std::shared_ptr<const ModelComponent<Type>>();
  }

  /**
   * @brief Find the first component that can be cast to ComponentType.
   */
  template <typename ComponentType>
  std::shared_ptr<ComponentType> FindComponentAs() {
    static_assert(std::is_base_of<ModelComponent<Type>, ComponentType>::value,
                  "ComponentType must inherit from ModelComponent<Type>");
    for (size_t i = 0; i < this->components.size(); i++) {
      std::shared_ptr<ComponentType> component =
          std::dynamic_pointer_cast<ComponentType>(this->components[i]);
      if (component) {
        return component;
      }
    }
    return std::shared_ptr<ComponentType>();
  }

  /**
   * @brief Find the first component that can be cast to ComponentType.
   */
  template <typename ComponentType>
  std::shared_ptr<const ComponentType> FindComponentAs() const {
    static_assert(std::is_base_of<ModelComponent<Type>, ComponentType>::value,
                  "ComponentType must inherit from ModelComponent<Type>");
    for (size_t i = 0; i < this->components.size(); i++) {
      std::shared_ptr<const ComponentType> component =
          std::dynamic_pointer_cast<const ComponentType>(this->components[i]);
      if (component) {
        return component;
      }
    }
    return std::shared_ptr<const ComponentType>();
  }

  /**
   * @brief Test whether the model contains a component with the requested name.
   */
  bool HasComponent(const std::string &component_name) const {
    return static_cast<bool>(this->FindComponent(component_name));
  }

  /**
   * @brief Test whether the model contains a component of the requested type.
   */
  template <typename ComponentType>
  bool HasComponentType() const {
    return static_cast<bool>(this->template FindComponentAs<ComponentType>());
  }

  /**
   * @brief Remove all components with the requested name.
   *
   * @return Number of components removed.
   */
  size_t RemoveComponents(const std::string &component_name) {
    size_t original_size = this->components.size();
    this->components.erase(
        std::remove_if(
            this->components.begin(), this->components.end(),
            [&component_name](const std::shared_ptr<ModelComponent<Type>>
                                  &component) {
              return component->GetName() == component_name;
            }),
        this->components.end());
    return original_size - this->components.size();
  }

  /**
   * @brief Replace the first component with the requested name.
   *
   * @return true if a component was replaced.
   */
  bool ReplaceComponent(const std::string &component_name,
                        std::shared_ptr<ModelComponent<Type>> component) {
    if (!component) {
      throw std::invalid_argument("ReplaceComponent: component is null");
    }
    for (size_t i = 0; i < this->components.size(); i++) {
      if (this->components[i]->GetName() == component_name) {
        this->components[i] = component;
        return true;
      }
    }
    return false;
  }

  /**
   * @brief Get the ordered names of model components.
   */
  std::vector<std::string> GetComponentNames() const {
    std::vector<std::string> component_names;
    component_names.reserve(this->components.size());
    for (size_t i = 0; i < this->components.size(); i++) {
      component_names.push_back(this->components[i]->GetName());
    }
    return component_names;
  }

  /**
   * @brief Run all model components for a lifecycle stage.
   */
  void RunComponents(ModelComponentStage stage) {
    ModelContext<Type> context = this->CreateContext();
    for (size_t i = 0; i < this->components.size(); i++) {
      switch (stage) {
        case ModelComponentStage::kInitialize:
          this->components[i]->Initialize(context);
          break;
        case ModelComponentStage::kPrepare:
          this->components[i]->Prepare(context);
          break;
        case ModelComponentStage::kEvaluate:
          this->components[i]->Evaluate(context);
          break;
        case ModelComponentStage::kReport:
          this->components[i]->Report(context);
          break;
      }
    }
  }

  /**
   * @brief Initialize a model.
   *
   */
  virtual void Initialize() {
    this->RunComponents(ModelComponentStage::kInitialize);
  }

  /**
   * @brief Prepare the model.
   *
   */
  virtual void Prepare() {
    this->RunComponents(ModelComponentStage::kPrepare);
  }

  /**
   * @brief Reset a vector from start to end with a value.
   *
   * @param v A vector to reset.
   * @param value The value you want to use for all elements in the
   * vector. The default is 0.0.
   */
  virtual void ResetVector(fims::Vector<Type> &v, Type value = 0.0) {
    std::fill(v.begin(), v.end(), value);
  }

  /**
   * @brief Evaluate the model.
   *
   */
  virtual void Evaluate() {
    this->RunComponents(ModelComponentStage::kEvaluate);
  }

  /**
   * @brief Report the model results via TMB.
   *
   */
  virtual void Report() {
    this->RunComponents(ModelComponentStage::kReport);
  }

  /**
   * @brief Get the Id object.
   *
   * @return uint32_t
   */
  uint32_t GetId() { return this->id; }
};

template <typename Type>
uint32_t FisheryModelBase<Type>::id_g = 0;

}  // namespace fims_popdy
#endif
