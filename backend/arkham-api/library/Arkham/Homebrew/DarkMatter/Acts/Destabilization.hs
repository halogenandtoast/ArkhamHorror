module Arkham.Homebrew.DarkMatter.Acts.Destabilization (destabilization) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (ModifierType (IgnoreRevelation))
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawFacedownCardWith, getFacedownCards)
import Arkham.I18n
import Arkham.Matcher
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
    [ restricted
        a
        1
        (exists $ HasMatchingTreachery (TreacheryFacedownInThreatAreaOf You) <> You)
        actionAbility
    , restricted a 2 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage Destabilization where
  runMessage msg a@(Destabilization attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      facedown <- getFacedownCards iid
      chooseOrRunOneM iid $ targets facedown \tid -> drawFacedownCardWith iid tid do
        -- The rider is offered with the card face up but before its revelation
        -- is initiated: "cancel" has to interrupt the effect's initiation.
        -- IgnoreRevelation is the engine's revelation cancel; ResolveTreachery
        -- reads it and discards the treachery unresolved, which is what the
        -- rules require ("the card is still regarded as having been drawn, and
        -- it is still placed in the encounter discard pile").
        clues <- getSpendableClueCount [iid]
        when (clues > 0) do
          card <- field TreacheryCard tid
          chooseOneM iid $ withI18n do
            labeled' "cancelRevelationEffect" do
              spendClues iid 1
              cardResolutionModifier card (attrs.ability 1) (CardIdTarget card.id) IgnoreRevelation
              checkAfter
                $ Window.CancelledOrIgnoredCardOrGameEffect
                  (toSource $ attrs.ability 1)
                  (Just card.id)
            countVar 1 $ labeled' "doNotSpendClues" nothing
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> Destabilization <$> liftRunMessage msg attrs
