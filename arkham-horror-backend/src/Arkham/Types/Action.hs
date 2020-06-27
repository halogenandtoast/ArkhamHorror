module Arkham.Types.Action where

import Arkham.Types.Card
import Arkham.Types.Location
import ClassyPrelude
import Data.Aeson
import Data.Aeson.Casing

data ArkhamDrawCardAction = ArkhamDrawCardAction
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamTakeResourceAction = ArkhamTakeResourceAction
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype ArkhamPlayCardAction = ArkhamPlayCardAction { apcaCardIndex :: Int }
  deriving newtype (Show, ToJSON, FromJSON)

data ArkhamCardAbilityAction = ArkhamCardAbilityAction { acaaCard :: ArkhamCard, aacaActionIndex :: Int }
  deriving stock (Show, Generic)

instance ToJSON ArkhamCardAbilityAction where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

instance FromJSON ArkhamCardAbilityAction where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

data ArkhamMoveAction = ArkhamMoveAction { amaFrom :: ArkhamLocation , amaTo :: ArkhamLocation }
  deriving stock (Show, Generic)

instance ToJSON ArkhamMoveAction where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamMoveAction where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

newtype ArkhamInvestigateAction = ArkhamInvestigateAction { aiaLocationId :: ArkhamCardCode }
  deriving newtype (Show, ToJSON, FromJSON)

newtype ArkhamFightEnemyAction = ArkhamFightEnemyAction { afeaEnemy :: ArkhamCard }
  deriving newtype (Show, ToJSON, FromJSON)

newtype ArkhamEngageEnemyAction = ArkhamEngageEnemyAction { aeeaEnemy :: ArkhamCard }
  deriving newtype (Show, ToJSON, FromJSON)

newtype ArkhamEvadeEnemyAction = ArkhamEvadeEnemyAction { aeveaEnemy :: ArkhamCard }
  deriving newtype (Show, ToJSON, FromJSON)

data ArkhamAction
  -- {"tag":"DrawCardAction","contents":[]}
  = DrawCardAction ArkhamDrawCardAction
  -- {"tag":"TakeResourceAction","contents":[]}
  | TakeResourceAction ArkhamTakeResourceAction
  -- {"tag":"PlayCardAction","contents":{"tag":"PlayerCard","contents":{"image":"","cost":3,"name":"Machete"}}}
  | PlayCardAction ArkhamPlayCardAction
  -- {"tag":"CardAbilityAction","contents":{"card":{"tag":"PlayerCard","contents":{"image": "","cost":3,"name":"Machete"}},"actionIndex":1}}
  | CardAbilityAction ArkhamCardAbilityAction
  -- {"tag":"MoveAction","contents":{"to":{"tag":"RevealedLocation","contents":{"image":"","shroud":2,"name":"Study","locationId":"Study","locationSymbols":[]}},"from":{"tag":"RevealedLocation","contents":{"image":"","shroud":2,"name":"Study","locationId":"Study","locationSymbols":[]}}}}
  | MoveAction ArkhamMoveAction
  -- {"tag":"InvestigateAction","contents":"Study"}
  | InvestigateAction ArkhamInvestigateAction
  -- {"tag":"FightEnemyAction","contents":{"tag":"EncounterCard","contents":{"image":"","name":"Icy Ghoul"}}}
  | FightEnemyAction ArkhamFightEnemyAction
  -- {"tag":"EngageEnemyAction","contents":{"tag":"EncounterCard","contents":{"image":"","name":"Icy Ghoul"}}}
  | EngageEnemyAction ArkhamEngageEnemyAction
  -- {"tag":"EvadeEnemyAction","contents":{"tag":"EncounterCard","contents":{"image":"","name":"Icy Ghoul"}}}
  | EvadeEnemyAction ArkhamEvadeEnemyAction
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
