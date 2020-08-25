{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.WendyAdams where

import Arkham.Types.Ability
import Arkham.Types.AssetId
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.FastWindow
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype WendyAdams = WendyAdams Attrs
  deriving newtype (Show, ToJSON, FromJSON)

wendyAdams :: WendyAdams
wendyAdams = WendyAdams $ baseAttrs
  "01005"
  "Wendy Adams"
  Survivor
  Stats
    { health = 7
    , sanity = 7
    , willpower = 4
    , intellect = 3
    , combat = 1
    , agility = 4
    }
  [Drifter]

instance (ActionRunner env investigator) => HasActions env investigator WendyAdams where
  getActions i (WhenDrawToken You) (WendyAdams attrs@Attrs {..})
    | getId () i == investigatorId = do
      let
        ability = (mkAbility
                    (InvestigatorSource investigatorId)
                    1
                    (ReactionAbility (WhenDrawToken You))
                  )
          { abilityLimit = OncePerTestOrAbility
          }
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability)
          `notElem` usedAbilities
          && discardableCardCount i
          > 0
        ]
  getActions i window (WendyAdams attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env WendyAdams where
  runMessage msg i@(WendyAdams attrs@Attrs {..}) = case msg of
    When (DrawToken token) ->
      i <$ unshiftMessage (CheckFastWindow investigatorId [WhenDrawToken You])
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      maid <- asks (getId @(Maybe AssetId) (CardCode "01014"))
      case maid of
        Nothing -> i <$ runTest skillValue 0
        Just _ -> i <$ unshiftMessage PassSkillTest
    _ -> WendyAdams <$> runMessage msg attrs
