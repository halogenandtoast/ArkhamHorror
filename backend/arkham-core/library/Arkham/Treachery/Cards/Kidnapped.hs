module Arkham.Treachery.Cards.Kidnapped (kidnapped, Kidnapped (..)) where

import Arkham.Ability
import Arkham.Choose
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Kidnapped = Kidnapped TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kidnapped :: TreacheryCard Kidnapped
kidnapped = treachery Kidnapped Cards.kidnapped

instance HasAbilities Kidnapped where
  getAbilities (Kidnapped attrs) = case attrs.attached of
    Just (AgendaTarget aid) -> [mkAbility attrs 1 $ forced $ AgendaAdvances #when $ AgendaWithId aid]
    _ -> []

instance RunMessage Kidnapped where
  runMessage msg t@(Kidnapped attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseOne
        iid
        [ Label "Test {willpower} (4)" [Msg.revelationSkillTest sid iid attrs #willpower (Fixed 4)]
        , Label "Test {agility} (4)" [Msg.revelationSkillTest sid iid attrs #agility (Fixed 4)]
        ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ assetControlledBy iid <> #ally
      if null allies
        then assignDamage iid attrs 2
        else do
          chooseOne
            iid
            [ targetLabel aid [AddToScenarioDeck PotentialSacrifices (AssetTarget aid)]
            | aid <- allies
            ]
          selectJust AnyAgenda >>= attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ChooseFrom iid $ chooseRandom attrs PotentialSacrifices 1
      pure t
    ChoseCards _ chosen | isTarget attrs chosen.target -> do
      push $ PlaceUnderneath AgendaDeckTarget chosen.cards
      pure t
    _ -> Kidnapped <$> liftRunMessage msg attrs
