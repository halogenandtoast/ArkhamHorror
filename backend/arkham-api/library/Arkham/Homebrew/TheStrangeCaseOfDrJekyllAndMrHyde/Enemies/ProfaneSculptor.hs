{- | "While there is a treachery attached to a card in play, Profane Sculptor gets
+1 health. While there are two or more treacheries attached to cards in play,
Profane Sculptor gains +1 evade and alert." Two constant clauses, no printed
ability.

"Attached to a card in play" is counted as every attachment placement the engine
records -- location, enemy, asset, event, act, agenda, treachery, investigator --
plus a threat area, which sits on the investigator's card but which
'placementToAttached' does not report as an attachment on its own.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Enemies.ProfaneSculptor (profaneSculptor) where

import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Cards
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Types (Field (TreacheryPlacement))

newtype ProfaneSculptor = ProfaneSculptor EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

profaneSculptor :: EnemyCard ProfaneSculptor
profaneSculptor = enemy ProfaneSculptor Cards.profaneSculptor

isAttachedToACardInPlay :: Placement -> Bool
isAttachedToACardInPlay = \case
  InThreatArea _ -> True
  placement -> isJust (placementToAttached placement)

instance HasModifiersFor ProfaneSculptor where
  getModifiersFor (ProfaneSculptor a) = do
    placements <- map snd <$> selectWithField TreacheryPlacement InPlayTreachery
    let n = length $ filter isAttachedToACardInPlay placements
    modifySelfWhen a (n >= 1) [HealthModifier 1]
    modifySelfWhen a (n >= 2) [EnemyEvade 1, AddKeyword Keyword.Alert]
