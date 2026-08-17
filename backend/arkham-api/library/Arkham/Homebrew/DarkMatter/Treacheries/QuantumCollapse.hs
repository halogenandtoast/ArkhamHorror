module Arkham.Homebrew.DarkMatter.Treacheries.QuantumCollapse (quantumCollapse) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  drawAllFacedownCards,
  facedownDrawnEventFor,
  getFacedownCardCount,
 )
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted

newtype QuantumCollapse = QuantumCollapse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumCollapse :: TreacheryCard QuantumCollapse
quantumCollapse = treachery QuantumCollapse Cards.quantumCollapse

{- | "Revelation - Draw each face-down encounter card in your threat area, one at
a time. If no encounter cards were drawn by this effect, add Quantum Collapse to
your threat area, face-down.
Forced - After you draw Quantum Collapse from your threat area: Take 1 horror."

Only *this* copy being drawn triggers, so the identity check lives in the window
key. Matching the broad @drewFacedown@ key and comparing ids in the handler
instead offered every face-down copy a no-op forced ability after every face-down
draw — including draws of enemies and assets, whose payload is not even a
'TreacheryId'.
-}
instance HasAbilities QuantumCollapse where
  getAbilities (QuantumCollapse a) =
    [mkAbility a 1 $ forced $ ScenarioEvent #after (Just You) (facedownDrawnEventFor a.id)]

instance RunMessage QuantumCollapse where
  runMessage msg t@(QuantumCollapse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- getFacedownCardCount iid
      if n == 0
        then place attrs (FacedownInThreatArea iid)
        else drawAllFacedownCards iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    _ -> QuantumCollapse <$> liftRunMessage msg attrs
