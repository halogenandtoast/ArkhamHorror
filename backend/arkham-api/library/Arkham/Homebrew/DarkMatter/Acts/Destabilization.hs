module Arkham.Homebrew.DarkMatter.Acts.Destabilization (destabilization) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawFacedownCard, getFacedownCards)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Destabilization = Destabilization ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

destabilization :: ActCard Destabilization
destabilization = act (2, A) Destabilization Cards.destabilization Nothing

{- | "[action] Draw a face-down encounter card in your threat area. If it is a
treachery, you may spend 1 clue to cancel its revelation effect.
Objective - If each undefeated investigator has resigned, advance."

TODO(homebrew): the "spend 1 clue to cancel its revelation" rider is not
modeled; cancelling a revelation needs a cancel-window hook around the drawn
card's resolution.
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
      chooseOrRunOneM iid $ targets facedown $ drawFacedownCard iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> Destabilization <$> liftRunMessage msg attrs
