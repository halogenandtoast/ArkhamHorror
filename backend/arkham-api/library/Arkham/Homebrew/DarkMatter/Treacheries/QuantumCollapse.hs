module Arkham.Homebrew.DarkMatter.Treacheries.QuantumCollapse (quantumCollapse) where

import Arkham.Ability
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (drawFacedownCard, facedownDrawnEvent, getFacedownCards)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype QuantumCollapse = QuantumCollapse TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

quantumCollapse :: TreacheryCard QuantumCollapse
quantumCollapse = treachery QuantumCollapse Cards.quantumCollapse

{- | "Revelation - Draw each face-down encounter card in your threat area, one at
a time. If no encounter cards were drawn by this effect, add Quantum Collapse to
your threat area, face-down.
Forced - After you draw Quantum Collapse from your threat area: Take 1 horror."
-}
instance HasAbilities QuantumCollapse where
  getAbilities (QuantumCollapse a) =
    [mkAbility a 1 $ forced $ ScenarioEvent #after (Just You) facedownDrawnEvent]

drawnTreachery :: [Window] -> Maybe TreacheryId
drawnTreachery = \case
  (windowType -> Window.ScenarioEvent key _ v) : _ | key == facedownDrawnEvent -> Just (toResult v)
  _ : rest -> drawnTreachery rest
  [] -> Nothing

instance RunMessage QuantumCollapse where
  runMessage msg t@(QuantumCollapse attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      facedown <- filter (/= attrs.id) <$> getFacedownCards iid
      if null facedown
        then place attrs (FacedownInThreatArea iid)
        else for_ facedown $ drawFacedownCard iid
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 (drawnTreachery -> Just tid) _ | tid == attrs.id -> do
      assignHorror iid (attrs.ability 1) 1
      pure t
    _ -> QuantumCollapse <$> liftRunMessage msg attrs
