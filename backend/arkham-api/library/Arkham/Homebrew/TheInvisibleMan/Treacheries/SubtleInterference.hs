module Arkham.Homebrew.TheInvisibleMan.Treacheries.SubtleInterference (subtleInterference) where

import Arkham.Ability
import Arkham.Helpers.Window (getPlayedEvent)
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype SubtleInterference = SubtleInterference TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

subtleInterference :: TreacheryCard SubtleInterference
subtleInterference = treachery SubtleInterference Cards.subtleInterference

instance HasAbilities SubtleInterference where
  getAbilities (SubtleInterference a) =
    [ scenarioI18n
        $ withI18nTooltip "subtleInterference.interfere"
        $ restricted a 1 (InThreatAreaOf You)
        $ forced
        $ PlayEventDiscarding #after You ActiveEvent
    , scenarioI18n
        $ withI18nTooltip "subtleInterference.discard"
        $ restricted a 2 (InThreatAreaOf You)
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage SubtleInterference where
  runMessage msg t@(SubtleInterference attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseRevelationSkillTest sid iid attrs [#willpower, #agility] (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 ws@(getPlayedEvent -> eid) _ -> do
      fetchCardMaybe eid >>= traverse_ \card -> do
        push $ RemoveEvent eid
        cancelWindowBatch ws
        removeCardFromGame card
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> SubtleInterference <$> liftRunMessage msg attrs
