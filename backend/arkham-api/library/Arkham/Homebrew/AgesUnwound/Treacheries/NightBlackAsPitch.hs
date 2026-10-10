module Arkham.Homebrew.AgesUnwound.Treacheries.NightBlackAsPitch (nightBlackAsPitch) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype NightBlackAsPitch = NightBlackAsPitch TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nightBlackAsPitch :: TreacheryCard NightBlackAsPitch
nightBlackAsPitch = treachery NightBlackAsPitch Cards.nightBlackAsPitch

{- | "Each location gets +1 shroud. / Each enemy gets +1 evade. / Increase all
horror taken by 1."
-}
instance HasModifiersFor NightBlackAsPitch where
  getModifiersFor (NightBlackAsPitch a) = do
    modifySelect a Anywhere [ShroudModifier 1]
    modifySelect a AnyEnemy [EnemyEvade 1]
    modifySelect a UneliminatedInvestigator [HorrorTaken 1]

-- | "Forced - At the end of the round: Discard Night Black as Pitch."
instance HasAbilities NightBlackAsPitch where
  getAbilities (NightBlackAsPitch a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage NightBlackAsPitch where
  runMessage msg t@(NightBlackAsPitch attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      placeTreachery attrs NextToAgenda
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> NightBlackAsPitch <$> liftRunMessage msg attrs
