{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Investigator.Cards.RexMurphy
  ( RexMurphy(..)
  , rexMurphy
  )
where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype RexMurphy = RexMurphy Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env RexMurphy where
  getModifiersFor source target (RexMurphy attrs) =
    getModifiersFor source target attrs

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

instance ActionRunner env => HasActions env RexMurphy where
  getActions iid (AfterPassSkillTest source You n) (RexMurphy attrs@Attrs {..})
    | iid == investigatorId && n >= 2 = do
      let
        ability = mkAbility
          (toSource attrs)
          1
          (ReactionAbility (AfterPassSkillTest source You n))
      clueCount' <- unClueCount <$> getCount investigatorLocation
      usedAbilities <- map unUsedAbility <$> getList ()
      pure
        [ ActivateCardAbilityAction investigatorId ability
        | (investigatorId, ability) `notElem` usedAbilities && clueCount' > 0
        ]
  getActions i window (RexMurphy attrs) = getActions i window attrs

instance HasTokenValue env RexMurphy where
  getTokenValue (RexMurphy attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 2)
  getTokenValue (RexMurphy attrs) iid token = getTokenValue attrs iid token

instance (InvestigatorRunner env) => RunMessage env RexMurphy where
  runMessage msg i@(RexMurphy attrs@Attrs {..}) = case msg of
    UseCardAbility _ (InvestigatorSource iid) _ 1 | iid == investigatorId ->
      i <$ unshiftMessage
        (DiscoverCluesAtLocation investigatorId investigatorLocation 1)
    ResolveToken _drawnToken ElderSign iid | iid == investigatorId ->
      i <$ unshiftMessage
        (Ask iid $ ChooseOne
          [ Label
            "Automatically fail to draw 3"
            [FailSkillTest, DrawCards iid 3 False]
          , Label "Resolve normally" []
          ]
        )
    _ -> RexMurphy <$> runMessage msg attrs
