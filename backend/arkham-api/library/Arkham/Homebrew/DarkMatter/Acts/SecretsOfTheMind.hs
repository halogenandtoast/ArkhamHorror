module Arkham.Homebrew.DarkMatter.Acts.SecretsOfTheMind (secretsOfTheMind) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern Simulation)
import Arkham.Matcher

newtype SecretsOfTheMind = SecretsOfTheMind ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsOfTheMind :: ActCard SecretsOfTheMind
secretsOfTheMind = act (2, A) SecretsOfTheMind Cards.secretsOfTheMind Nothing

{- | "Forced - If a [[Simulation]] location has no clues on it: Add it to the
victory display.
Objective - If each undefeated investigator has resigned, advance."
-}
instance HasAbilities SecretsOfTheMind where
  getAbilities (SecretsOfTheMind a) =
    [ restricted a 1 (exists $ LocationWithTrait Simulation <> LocationWithoutClues)
        $ forced AnyWindow
    , restricted a 2 (not_ $ exists $ UneliminatedInvestigator <> not_ ResignedInvestigator)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage SecretsOfTheMind where
  runMessage msg a@(SecretsOfTheMind attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cleared <- select $ LocationWithTrait Simulation <> LocationWithoutClues
      for_ cleared $ addToVictory iid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SecretsOfTheMind <$> liftRunMessage msg attrs
