module Arkham.Homebrew.DarkMatter.Acts.Awakening (awakening) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype Awakening = Awakening ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakening :: ActCard Awakening
awakening = act (1, A) Awakening Cards.awakening Nothing

{- | "As an additional cost to enter Backstage, investigators in the Theatre must
spend 2[per_investigator] clues, as a group."

Backstage and the Theatre have no card definitions in this set, so they are
matched by title.
-}
instance HasModifiersFor Awakening where
  getModifiersFor (Awakening a) =
    modifySelect
      a
      (InvestigatorAt "Theatre")
      [AdditionalCostToEnterMatching "Backstage" (GroupClueCost (PerPlayer 2) "Theatre")]

-- | "Objective - After an investigator reveals the Backstage, advance."
instance HasAbilities Awakening where
  getAbilities (Awakening a) =
    [ restricted a 1 (exists $ LocationWithTitle "Backstage" <> RevealedLocation)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage Awakening where
  runMessage msg a@(Awakening attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> Awakening <$> liftRunMessage msg attrs
