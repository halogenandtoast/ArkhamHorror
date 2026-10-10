module Arkham.Homebrew.AgesUnwound.Acts.AgainstAGod (againstAGod) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype AgainstAGod = AgainstAGod ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 3. The clue requirement is declared so the card prints it, but the
advance is /not/ the engine's own free objective (ability 999, available during
any investigator's turn): this act only lets the clues be spent at the end of the
round, which is ability 1 below. 'getAbilities' therefore replaces the attrs'
list rather than extending it.
-}
againstAGod :: ActCard AgainstAGod
againstAGod =
  act (3, A) AgainstAGod Cards.againstAGod (Just $ GroupClueCost (PerPlayer 4) Anywhere)

{- | "__Objective__ - At the end of the round, investigators may spend the
requisite number of clues, as a group, to advance."

"May", so a reaction rather than a __Forced__: the table is offered the spend in
the round-end window and can decline it.
-}
instance HasAbilities AgainstAGod where
  getAbilities (AgainstAGod a) =
    [ mkAbility a 1
        $ Objective
        $ triggered (RoundEnds #when) (GroupClueCost (PerPlayer 4) Anywhere)
    ]

-- | Act 3b /One Last Offensive/ is flavour only.
instance RunMessage AgainstAGod where
  runMessage msg a@(AgainstAGod attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ AdvanceAct attrs.id (InvestigatorSource iid) AdvancedWithClues
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> AgainstAGod <$> liftRunMessage msg attrs
