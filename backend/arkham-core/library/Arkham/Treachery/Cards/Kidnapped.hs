module Arkham.Treachery.Cards.Kidnapped (kidnapped, Kidnapped (..)) where

import Arkham.Ability
import Arkham.Choose
import Arkham.Classes
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Kidnapped = Kidnapped TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kidnapped :: TreacheryCard Kidnapped
kidnapped = treachery Kidnapped Cards.kidnapped

instance HasAbilities Kidnapped where
  getAbilities (Kidnapped attrs) = case treacheryAttachedTarget attrs of
    Just (AgendaTarget aid) -> [mkAbility attrs 1 $ forced $ AgendaAdvances #when $ AgendaWithId aid]
    _ -> []

instance RunMessage Kidnapped where
  runMessage msg t@(Kidnapped attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label "Test {willpower} (4)" [revelationSkillTest iid attrs #willpower (Fixed 4)]
          , Label "Test {agility} (4)" [revelationSkillTest iid attrs #agility (Fixed 4)]
          ]
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ assetControlledBy iid <> #ally
      if null allies
        then push $ assignDamage iid attrs 2
        else do
          agendaId <- selectJust AnyAgenda
          player <- getPlayer iid
          pushAll
            [ chooseOne
                player
                [ targetLabel aid [AddToScenarioDeck PotentialSacrifices (AssetTarget aid)]
                | aid <- allies
                ]
            , AttachTreachery treacheryId (AgendaTarget agendaId)
            ]
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ ChooseFrom iid $ chooseRandom attrs PotentialSacrifices 1
      pure t
    ChoseCards _ chosen | isTarget attrs chosen.target -> do
      push $ PlaceUnderneath AgendaDeckTarget chosen.cards
      pure t
    _ -> Kidnapped <$> runMessage msg attrs
