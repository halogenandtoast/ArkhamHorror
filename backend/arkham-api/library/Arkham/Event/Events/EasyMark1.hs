module Arkham.Event.Events.EasyMark1 (easyMark1, easyMark1Effect) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype EasyMark1 = EasyMark1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easyMark1 :: EventCard EasyMark1
easyMark1 = event EasyMark1 Cards.easyMark1

-- This one will never trigger, only the effect version below
instance HasAbilities EasyMark1 where
  getAbilities (EasyMark1 a) =
    [ controlled a 1 (youExist $ handWith Cards.easyMark1)
        $ freeReaction (Arkham.Matcher.PlayEvent #after You (be a))
    ]

instance RunMessage EasyMark1 where
  runMessage msg e@(EasyMark1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResources iid attrs 2
      drawCards iid attrs 1
      createCardEffect Cards.easyMark1 Nothing attrs iid
      pure $ overAttrs (waitingL .~ True) e
    _ -> EasyMark1 <$> liftRunMessage msg attrs

newtype EasyMark1Effect = EasyMark1Effect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easyMark1Effect :: EffectArgs -> EasyMark1Effect
easyMark1Effect = cardEffect EasyMark1Effect Cards.easyMark1

instance HasAbilities EasyMark1Effect where
  getAbilities (EasyMark1Effect a) = fromMaybe [] do
    iid <- a.target.investigator
    eid <- a.source.event
    pure
      [ displayAsCard
          $ withTooltip
            "{reaction}: After you play Easy Mark: Play another Easy Mark from your hand, at no cost."
          $ restricted a 1 (exists $ InvestigatorWithId iid <> handWith Cards.easyMark1)
          $ freeReaction (Arkham.Matcher.PlayEvent #after (InvestigatorWithId iid) (EventWithId eid))
      ]

instance RunMessage EasyMark1Effect where
  runMessage msg e@(EasyMark1Effect attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mcard <- selectOne $ inHandOf ForPlay iid <> basic (cardIs Cards.easyMark1)
      for_ mcard $ putCardIntoPlay iid
      pure e
    _ -> EasyMark1Effect <$> liftRunMessage msg attrs
