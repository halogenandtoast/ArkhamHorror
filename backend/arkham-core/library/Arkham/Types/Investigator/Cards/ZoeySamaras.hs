{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.ZoeySamaras where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window (Who(..))
import qualified Arkham.Types.Window as Fast
import ClassyPrelude
import Data.Aeson

newtype ZoeySamaras = ZoeySamaras Attrs
  deriving newtype (Show, ToJSON, FromJSON)

zoeySamaras :: ZoeySamaras
zoeySamaras = ZoeySamaras $ baseAttrs
  "02001"
  "Zoey Samaras"
  Guardian
  Stats
    { health = 9
    , sanity = 6
    , willpower = 4
    , intellect = 2
    , combat = 4
    , agility = 2
    }
  [Believer, Hunter]

instance (ActionRunner env investigator) => HasActions env investigator ZoeySamaras where
  getActions i (Fast.AfterEnemyEngageInvestigator You eid) (ZoeySamaras Attrs {..})
    | getId () i == investigatorId
    = do
      let
        ability = mkAbility
          (InvestigatorSource investigatorId)
          1
          (ReactionAbility (Fast.AfterEnemyEngageInvestigator You eid))

      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability) `notElem` usedAbilities
        ]

  getActions i window (ZoeySamaras attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env ZoeySamaras where
  runMessage msg i@(ZoeySamaras attrs@Attrs {..}) = case msg of
    UseCardAbility _ _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      i <$ unshiftMessage (TakeResources investigatorId 1 False)
    ResolveToken ElderSign iid skillValue | iid == investigatorId -> do
      runTest investigatorId skillValue 1
      i <$ unshiftMessage
        (AddModifiers
          SkillTestTarget
          (InvestigatorSource investigatorId)
          [DamageDealt 1]
        )
    _ -> ZoeySamaras <$> runMessage msg attrs
