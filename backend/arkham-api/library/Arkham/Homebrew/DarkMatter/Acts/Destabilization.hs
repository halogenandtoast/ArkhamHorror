module Arkham.Homebrew.DarkMatter.Acts.Destabilization (destabilization) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (ModifierType (IgnoreRevelation))
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawRandomFacedownCardWith)
import Arkham.I18n
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Types (Field (TreacheryCard))
import Arkham.Window qualified as Window

newtype Destabilization = Destabilization ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

destabilization :: ActCard Destabilization
destabilization = act (2, A) Destabilization Cards.destabilization Nothing

{- | "[action] Draw a face-down encounter card in your threat area. If it is a
treachery, you may spend 1 clue to cancel its revelation effect.
Objective - If each undefeated investigator has resigned, advance."
-}
instance HasAbilities Destabilization where
  getAbilities (Destabilization a) =
    [ restricted a 1 (DuringTurn You) actionAbility
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage Destabilization where
  runMessage msg a@(Destabilization attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- The rider is offered with the card face up but before its revelation is
      -- initiated. IgnoreRevelation cancels that effect; the card was still
      -- drawn and is discarded normally.
      void $ drawRandomFacedownCardWith iid \tid -> do
        clues <- getSpendableClueCount [iid]
        card <- field TreacheryCard tid
        -- Call of the Void prints "Cannot be cancelled or ignored", and Schröd
        -- Generators stops the investigators on it cancelling anything at all.
        canCancel <- card <=~> CanCancelRevelationEffect (InvestigatorWithId iid) #any
        when (clues > 0 && canCancel) do
          chooseOneM iid $ withI18n do
            labeled' "cancelRevelationEffect" do
              spendClues iid 1
              cardResolutionModifier card (attrs.ability 1) (CardIdTarget card.id) IgnoreRevelation
              checkAfter
                $ Window.CancelledOrIgnoredCardOrGameEffect (toSource $ attrs.ability 1) (Just card.id)
            countVar 1 $ labeled' "doNotSpendClues" nothing
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    -- Advancing is the campaign guide's "no resolution was reached (each
    -- investigator resigned or was defeated)", which the scenario then branches
    -- to Resolution 3 or 1 on whether Erwin was resigned with.
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      noResolution
      pure a
    _ -> Destabilization <$> liftRunMessage msg attrs
