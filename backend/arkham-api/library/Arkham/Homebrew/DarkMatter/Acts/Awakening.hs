module Arkham.Homebrew.DarkMatter.Acts.Awakening (awakening) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Matcher

newtype Awakening = Awakening ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakening :: ActCard Awakening
awakening = act (1, A) Awakening Cards.awakening Nothing

instance HasModifiersFor Awakening where
  getModifiersFor (Awakening a) =
    modifySelect
      a
      (InvestigatorAt "Theatre")
      [AdditionalCostToEnterMatching "Backstage" (GroupClueCost (PerPlayer 2) "Theatre")]

instance HasAbilities Awakening where
  getAbilities (Awakening a) =
    [ onlyOnce
        $ restricted a 1 (exists $ LocationWithTitle "Backstage" <> RevealedLocation)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage Awakening where
  runMessage msg a@(Awakening attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      spawnEnemy_ =<< genCard Enemies.theStranger
      advanceActDeck attrs
      pure a
    _ -> Awakening <$> liftRunMessage msg attrs
