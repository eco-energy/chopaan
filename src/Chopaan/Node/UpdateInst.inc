#define UpdateInstances(ty) \
deriving instance Eq       (ty 'Valid); \
deriving instance Ord      (ty 'Valid); \
deriving instance Show     (ty 'Valid); \
deriving instance ToJSON   (ty 'Valid); \
deriving instance FromJSON (ty 'Valid); \
deriving instance Eq       (ty 'Edit); \
deriving instance Ord      (ty 'Edit); \
deriving instance Show     (ty 'Edit); \
deriving instance ToJSON   (ty 'Edit); \
deriving instance FromJSON (ty 'Edit); \
deriving instance Show     (ty 'Errors);
