module Arkham.Homebrew.TheMasqueOfTheRedDeath.Treacheries.ShadowOfDeath (shadowOfDeath) where

import Arkham.Ability
import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (describedSkullEffect)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype ShadowOfDeath = ShadowOfDeath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowOfDeath :: TreacheryCard ShadowOfDeath
shadowOfDeath = treachery ShadowOfDeath Cards.shadowOfDeath

instance HasAbilities ShadowOfDeath where
  getAbilities (ShadowOfDeath a) = case a.placement of
    -- "Your location gains: '[skull]: -1. If you fail, discard a card from your
    -- hand at random.'" and "[reaction] After an enemy at your location is
    -- defeated: Discard Shadow of Death."
    InThreatArea iid ->
      [ describedSkullEffect
          (-1)
          "If you fail, discard a card from your hand at random."
          (proxied (locationWithInvestigator iid) a)
          1
      , restricted a 2 (InThreatAreaOf You)
          $ freeReaction (EnemyDefeated #after Anyone ByAny $ at_ YourLocation)
      ]
    _ -> []

instance RunMessage ShadowOfDeath where
  runMessage msg t@(ShadowOfDeath attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) sid
          $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier 1))
        onFailedByEffect sid (atLeast 0) (attrs.ability 1) iid
          $ randomDiscard iid (attrs.ability 1)
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ShadowOfDeath <$> liftRunMessage msg attrs
