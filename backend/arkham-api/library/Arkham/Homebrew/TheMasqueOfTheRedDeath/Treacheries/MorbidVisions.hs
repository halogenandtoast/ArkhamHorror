module Arkham.Homebrew.TheMasqueOfTheRedDeath.Treacheries.MorbidVisions (morbidVisions) where

import Arkham.Ability
import Arkham.ChaosToken (pattern NegativeModifier)
import Arkham.ChaosToken.Types (ChaosTokenValue (ChaosTokenValue))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.TheMasqueOfTheRedDeath.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheMasqueOfTheRedDeath.Helpers (describedSkullEffect)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype MorbidVisions = MorbidVisions TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morbidVisions :: TreacheryCard MorbidVisions
morbidVisions = treachery MorbidVisions Cards.morbidVisions

instance HasAbilities MorbidVisions where
  -- The threat-area investigator comes off the placement: an encounter treachery
  -- has no owner, that field is only set for weaknesses.
  getAbilities (MorbidVisions a) = case a.placement of
    -- "your location gains: '[skull]: -1. If you fail, take 1 horror.'" and
    -- "At the end of the round, discard Morbid Visions."
    InThreatArea iid ->
      [ describedSkullEffect (-1) "If you fail, take 1 horror." (proxied (locationWithInvestigator iid) a) 1
      , restricted a 2 (InThreatAreaOf You) $ forced $ RoundEnds #when
      ]
    _ -> []

instance RunMessage MorbidVisions where
  runMessage msg t@(MorbidVisions attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 1
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled "takeHorror" $ assignHorror iid attrs 1
        labeled "putItIntoPlay" $ placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 1) sid
          $ AddChaosTokenValue (ChaosTokenValue #skull (NegativeModifier 1))
        onFailedByEffect sid (atLeast 0) (attrs.ability 1) iid
          $ assignHorror iid (attrs.ability 1) 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> MorbidVisions <$> liftRunMessage msg attrs
