{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.RexMurphy where

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window
import ClassyPrelude
import Data.Aeson

newtype RexMurphy = RexMurphy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

rexMurphy :: RexMurphy
rexMurphy = RexMurphy $ baseAttrs
  "02002"
  "Rex Murphy"
  Seeker
  Stats
    { health = 6
    , sanity = 9
    , willpower = 3
    , intellect = 4
    , combat = 2
    , agility = 3
    }
  [Reporter]

instance (ActionRunner env investigator) => HasActions env investigator RexMurphy where
  getActions i (AfterPassSkillTest You n) (RexMurphy Attrs {..})
    | getId () i == investigatorId && n >= 2 = do
      let
        ability = mkAbility
          (InvestigatorSource investigatorId)
          1
          (ReactionAbility (AfterPassSkillTest You n))
      clueCount' <- unClueCount <$> asks (getCount investigatorLocation)
      usedAbilities <- map unUsedAbility <$> asks (getList ())
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability) `notElem` usedAbilities && clueCount' > 0
        ]
  getActions i window (RexMurphy attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env RexMurphy where
  runMessage msg i@(RexMurphy attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      i <$ unshiftMessage
        (DiscoverCluesAtLocation investigatorId investigatorLocation 1)
    ResolveToken ElderSign iid | iid == investigatorId -> i <$ unshiftMessage
      (Ask iid $ ChooseOne
        [ Label
          "Automatically fail to draw 3"
          [FailSkillTest, DrawCards iid 3 False]
        , Label "Resolve normally" [RunSkillTest iid [TokenValue ElderSign 2]]
        ]
      )
    _ -> RexMurphy <$> runMessage msg attrs
