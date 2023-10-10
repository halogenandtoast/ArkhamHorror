module Arkham.Event.Cards.TruthFromFiction (
  truthFromFiction,
  TruthFromFiction (..),
) where

import Arkham.Prelude

import Arkham.Asset.Uses qualified as Uses
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Matcher

newtype TruthFromFiction = TruthFromFiction EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

truthFromFiction :: EventCard TruthFromFiction
truthFromFiction = event TruthFromFiction Cards.truthFromFiction

instance RunMessage TruthFromFiction where
  runMessage msg e@(TruthFromFiction attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      assets <- selectList $ assetControlledBy iid <> AssetWithUseType Uses.Secret
      player <- getPlayer iid
      push $ chooseOne player [targetLabel aid [AddUses aid Uses.Secret 2] | aid <- assets]
      pure e
    _ -> TruthFromFiction <$> runMessage msg attrs
