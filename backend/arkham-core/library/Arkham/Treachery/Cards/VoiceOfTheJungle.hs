module Arkham.Treachery.Cards.VoiceOfTheJungle
  ( voiceOfTheJungle
  , VoiceOfTheJungle(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VoiceOfTheJungle = VoiceOfTheJungle TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

voiceOfTheJungle :: TreacheryCard VoiceOfTheJungle
voiceOfTheJungle = treachery VoiceOfTheJungle Cards.voiceOfTheJungle

instance HasAbilities VoiceOfTheJungle where
  getAbilities (VoiceOfTheJungle x) =
    [ restrictedAbility
        x
        1
        (InThreatAreaOf You
        <> InvestigatorExists (You <> NoSuccessfulExploreThisTurn)
        )
      $ ForcedAbility
      $ TurnEnds Timing.When You
    , restrictedAbility x 2 (InThreatAreaOf You)
      $ ActionAbility Nothing
      $ ActionCost 1
    ]

instance RunMessage VoiceOfTheJungle where
  runMessage msg t@(VoiceOfTheJungle attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      t <$ push (AttachTreachery (toId attrs) $ InvestigatorTarget iid)
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      t <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      push $ BeginSkillTest
        iid
        source
        (InvestigatorTarget iid)
        Nothing
        SkillWillpower
        3
      pure t
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> do
        push $ Discard (toTarget attrs)
        pure t
    _ -> VoiceOfTheJungle <$> runMessage msg attrs
