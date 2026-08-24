module Arkham.Homebrew.DarkMatter.Treacheries.Contamination (contamination) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (campaignI18n)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype Contamination = Contamination TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

contamination :: TreacheryCard Contamination
contamination = treachery Contamination Cards.contamination

{- | "Revelation - Attach Contamination to an [[Ally]] asset at your location
(unique if possible).
Forced - At the end of the round, you must either (choose one): Discard attached
[[Ally]], or place 1 doom on it."
-}
instance HasAbilities Contamination where
  getAbilities (Contamination a) =
    [restricted a 1 (youExist $ be a.drawnBy) $ forced $ RoundEnds #when]

instance RunMessage Contamination where
  runMessage msg t@(Contamination attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      unique' <- select $ #ally <> UniqueAsset <> AssetAt (locationWithInvestigator iid)
      allies <-
        if null unique'
          then select $ #ally <> AssetAt (locationWithInvestigator iid)
          else pure unique'
      if null allies
        then toDiscard attrs attrs
        else chooseOrRunOneM iid $ targets allies $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached \case
        AssetTarget aid -> chooseOneM iid $ campaignI18n do
          labeled' "contamination.discardAlly" $ toDiscardBy iid (attrs.ability 1) aid
          labeled' "contamination.placeDoom" $ placeDoom (attrs.ability 1) aid 1
        _ -> pure ()
      pure t
    _ -> Contamination <$> liftRunMessage msg attrs
