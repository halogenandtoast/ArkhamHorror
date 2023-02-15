module Arkham.Location.Cards.StepsOfYhagharl
  ( stepsOfYhagharl
  , StepsOfYhagharl(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( stepsOfYhagharl )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Types ( Field (..) )
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype StepsOfYhagharl = StepsOfYhagharl LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stepsOfYhagharl :: LocationCard StepsOfYhagharl
stepsOfYhagharl =
  location StepsOfYhagharl Cards.stepsOfYhagharl 3 (PerPlayer 1)

instance HasAbilities StepsOfYhagharl where
  getAbilities (StepsOfYhagharl attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
          $ ForcedAbility
          $ Leaves Timing.When You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage StepsOfYhagharl where
  runMessage msg l@(StepsOfYhagharl attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      encounterDiscard <- scenarioField ScenarioDiscard
      let
        mMadnessCard = find (member Madness . toTraits) encounterDiscard
        revelationMsgs = case mMadnessCard of
          Nothing -> []
          Just madnessCard ->
            [ RemoveFromEncounterDiscard madnessCard
            , InvestigatorDrewEncounterCard iid madnessCard
            ]
      pushAll revelationMsgs
      StepsOfYhagharl <$> runMessage msg attrs
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ beginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        2
      pure l
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        replaceMessageMatching
          (\case
            MoveFrom _ iid' lid' -> iid == iid' && lid' == toId attrs
            _ -> False
          )
          (const [])
        replaceMessageMatching
          (\case
            Will (MoveTo _ iid' _) | iid == iid' -> True
            _ -> False
          )
          (const [])
        replaceMessageMatching
          (\case
            After (Move _ iid' _) | iid == iid' -> True
            _ -> False
          )
          (const [])
        l <$ replaceMessageMatching
          (\case
            MoveTo _ iid' _ | iid == iid' -> True
            _ -> False
          )
          (const [ShuffleBackIntoEncounterDeck $ toTarget attrs])
    _ -> StepsOfYhagharl <$> runMessage msg attrs
