module Arkham.Api.Handler.Actions where

import Arkham.Fixtures
import Arkham.Types
import Data.Aeson
import Data.Aeson.Casing
import Import
import Lens.Micro

data ArkhamDrawCardAction = ArkhamDrawCardAction
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data ArkhamTakeResourceAction = ArkhamTakeResourceAction
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ArkhamPlayCardAction = ArkhamPlayCardAction { apcaCard :: ArkhamCard }
  deriving newtype (ToJSON, FromJSON)

data ArkhamCardAbilityAction = ArkhamCardAbilityAction { acaaCard :: ArkhamCard, aacaActionIndex :: Int }
  deriving stock (Generic)

instance ToJSON ArkhamCardAbilityAction where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

instance FromJSON ArkhamCardAbilityAction where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 4 }

data ArkhamMoveAction = ArkhamMoveAction { amaFrom :: ArkhamLocation , amaTo :: ArkhamLocation }
  deriving stock (Generic)

instance ToJSON ArkhamMoveAction where
  toJSON =
    genericToJSON $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }
  toEncoding = genericToEncoding
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

instance FromJSON ArkhamMoveAction where
  parseJSON = genericParseJSON
    $ defaultOptions { fieldLabelModifier = camelCase . drop 3 }

newtype ArkhamInvestigateAction = ArkhamInvestigateAction { aiaLocationId :: LocationId }
  deriving newtype (ToJSON, FromJSON)

newtype ArkhamFightEnemyAction = ArkhamFightEnemyAction { afeaEnemy :: ArkhamCard }
  deriving newtype (ToJSON, FromJSON)

newtype ArkhamEngageEnemyAction = ArkhamEngageEnemyAction { aeeaEnemy :: ArkhamCard }
  deriving newtype (ToJSON, FromJSON)

newtype ArkhamEvadeEnemyAction = ArkhamEvadeEnemyAction { aeveaEnemy :: ArkhamCard }
  deriving newtype (ToJSON, FromJSON)

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
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

applyAction :: ArkhamGame -> ArkhamAction -> IO ArkhamGame
applyAction g (InvestigateAction action) = do
  let mlocation = findLocation $ g ^. locations
  -- TODO: mark location, initiate intellect skill check against shroud
  -- skill check timing
  -- determine skill of test
  -- player window for fast actions
  -- Commit cards from hand
  -- player window
  -- Reveal token
  -- resolve token effects
  -- determine modified skill value
  -- determine success or failure
  -- apply results
  -- skill test ends
  pure g
 where
  targetLocationId = aiaLocationId action
  findLocation = find ((== targetLocationId) . (^. locationId))
applyAction g _ = pure g

postApiV1ArkhamGameActionR :: Int -> Handler ArkhamGame
postApiV1ArkhamGameActionR gameId = do
  game <- liftIO $ loadGameFixture gameId
  action <- requireCheckJsonBody
  liftIO $ applyAction game action
