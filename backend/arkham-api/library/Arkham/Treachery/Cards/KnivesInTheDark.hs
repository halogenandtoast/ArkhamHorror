module Arkham.Treachery.Cards.KnivesInTheDark (knivesInTheDark) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype KnivesInTheDark = KnivesInTheDark TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knivesInTheDark :: TreacheryCard KnivesInTheDark
knivesInTheDark = treachery KnivesInTheDark Cards.knivesInTheDark

instance HasAbilities KnivesInTheDark where
  getAbilities (KnivesInTheDark a) =
    [ playerLimit PerRound
        $ restricted a 1 InYourThreatArea
        $ forced
        $ CampaignEvent #after (Just You) "exposed[decoy]"
    , restricted a 2 InYourThreatArea $ forced $ TurnEnds #when You
    ]

instance RunMessage KnivesInTheDark where
  runMessage msg t@(KnivesInTheDark attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasKnivesInTheDark <-
        selectAny $ treacheryIs Cards.knivesInTheDark <> Arkham.Matcher.treacheryInThreatAreaOf iid
      if hasKnivesInTheDark
        then gainSurge attrs
        else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid attrs 2
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> KnivesInTheDark <$> liftRunMessage msg attrs
