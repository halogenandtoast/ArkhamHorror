module Arkham.Homebrew.AgesUnwound.Locations.ArkhamMassachusetts_086 (arkhamMassachusetts_086) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (CannotEnter), modifySelect)
import Arkham.Card.CardType
import Arkham.Helpers.Query (getPlayerCount, getSetAsideCardsMatching)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (Elite))

newtype ArkhamMassachusetts_086 = ArkhamMassachusetts_086 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /Arkham, Massachusetts (Present Day?)/ -- home, and the way out of the
scenario. Set aside at setup and put into play unrevealed by act 1b, alongside
The Timestream.
-}
arkhamMassachusetts_086 :: LocationCard ArkhamMassachusetts_086
arkhamMassachusetts_086 =
  location ArkhamMassachusetts_086 Cards.arkhamMassachusetts_086 3 (Static 0)

-- | Unrevealed: "You cannot enter Arkham, Massachusetts."
instance HasModifiersFor ArkhamMassachusetts_086 where
  getModifiersFor (ArkhamMassachusetts_086 a) =
    whenUnrevealed a $ modifySelect a Anyone [CannotEnter a.id]

{- | Revealed: "Forced - When Arkham, Massachusetts is revealed: Randomly choose
1 of the set-aside [[Elite]] enemies and spawn it at Arkham, Massachusetts (if
there are 3 or 4 investigators in the game, also spawn 1 at The Timestream)." /
"[action] If there are no ready enemies at Arkham, Massachusetts: __Resign.__ You
rejoin your time."

Unrevealed: "The Timestream gains '__Forced__ - At the end of the round, if each
undefeated investigator is at this location: Reveal Arkham, Massachusetts
/(Present Day?)/.'" Nothing in the engine grants one card another card's
ability, so the grant lives here as ability 2: same condition, same effect, and
the only observable difference is which card the log credits.
-}
instance HasAbilities ArkhamMassachusetts_086 where
  getAbilities (ArkhamMassachusetts_086 a) =
    extend a
      $ if a.revealed
        then
          [ mkAbility a 1 $ forced $ RevealLocation #when Anyone (be a)
          , restrict (notExists $ ReadyEnemy <> enemyAt a.id) $ locationResignAction a
          ]
        else
          [ restricted
              a
              2
              ( exists (locationIs Cards.theTimestream)
                  <> notExists
                    (UneliminatedInvestigator <> not_ (at_ $ locationIs Cards.theTimestream))
              )
              $ forced
              $ RoundEnds #when
          ]

instance RunMessage ArkhamMassachusetts_086 where
  runMessage msg l@(ArkhamMassachusetts_086 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      {- The three set-aside Elites were shuffled face down at setup, so the
      "random" choice is over whatever is still out of play. Entering play
      obtains the card, which is what keeps the second spawn from picking the
      same one. -}
      elites <- shuffleM =<< getSetAsideCardsMatching (CardWithType EnemyType <> CardWithTrait Elite)
      n <- getPlayerCount
      for_ (take 1 elites) \card -> createEnemyAt_ card attrs
      when (n >= 3) do
        selectOne (locationIs Cards.theTimestream) >>= traverse_ \lid ->
          for_ (take 1 $ drop 1 elites) \card -> createEnemyAt_ card lid
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      reveal attrs
      pure l
    _ -> ArkhamMassachusetts_086 <$> liftRunMessage msg attrs
