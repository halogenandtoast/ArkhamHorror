{- | "__Revelation__ - If there are no revealed Method cards at your location,
Reenactment gains surge. Otherwise, choose a Method card at your location. Test a
skill matching an icon on the chosen card (2) (you may choose any skill if that
icon is {wild}). If you fail, take 1 damage and Reenactment gains surge."

A facedown [[Method]] is a card beneath a location, not an asset, so
'methodAsset' already answers "revealed" on its own.
-}
module Arkham.Homebrew.AndThenThereWereNone.Treacheries.Reenactment (reenactment) where

import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType (SkillIcon (..), allSkills)
import Arkham.Treachery.Import.Lifted

newtype Reenactment = Reenactment TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reenactment :: TreacheryCard Reenactment
reenactment = treachery Reenactment Cards.reenactment

instance RunMessage Reenactment where
  runMessage msg t@(Reenactment attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      methods <- select $ methodAsset <> AssetAt (locationWithInvestigator iid)
      if null methods
        then gainSurge attrs
        else chooseTargetM iid methods \aid -> do
          sid <- getRandom
          -- A Method prints exactly one icon; {wild} is the only one that is not
          -- a skill of its own, and that is the clause offering a free choice.
          icons <- assetIcons aid
          case [sType | SkillIcon sType <- icons] of
            [sType] -> revelationSkillTest sid iid attrs sType (Fixed 2)
            _ -> chooseRevelationSkillTest sid iid attrs allSkills (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      gainSurge attrs
      pure t
    _ -> Reenactment <$> liftRunMessage msg attrs
