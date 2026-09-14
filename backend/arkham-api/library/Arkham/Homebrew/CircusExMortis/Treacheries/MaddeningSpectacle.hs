module Arkham.Homebrew.CircusExMortis.Treacheries.MaddeningSpectacle (maddeningSpectacle) where

import Arkham.Ability
import Arkham.Helpers.Investigator (getHighestSkillValues)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType
import Arkham.Treachery.Import.Lifted

newtype MaddeningSpectacle = MaddeningSpectacle TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maddeningSpectacle :: TreacheryCard MaddeningSpectacle
maddeningSpectacle = treachery MaddeningSpectacle Cards.maddeningSpectacle

instance HasModifiersFor MaddeningSpectacle where
  getModifiersFor (MaddeningSpectacle attrs) = runMaybeT_ do
    iid <- hoistMaybe attrs.inThreatAreaOf
    sk <- hoistMaybe $ maybeResult attrs.meta
    modified_ attrs iid [SkillModifier sk (-1)]

instance HasAbilities MaddeningSpectacle where
  getAbilities (MaddeningSpectacle x) = [restricted x 1 (InThreatAreaOf You) actionAbility]

instance RunMessage MaddeningSpectacle where
  runMessage msg t@(MaddeningSpectacle attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      (_, skills) <- getHighestSkillValues iid
      chooseOrRunOneM iid $ skillsLabeled skills (`forSkillType` msg)
      pure $ t & setMeta Null
    ForSkillType sk (Revelation _iid (isSource attrs -> True)) -> do
      pure $ t & setMeta (Just sk)
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      let others = allSkills & withMaybeResult attrs.meta id (\sk -> filter (/= sk))
      chooseBeginSkillTest sid iid (attrs.ability 1) iid others (Fixed 3)
      pure t
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> MaddeningSpectacle <$> liftRunMessage msg attrs
