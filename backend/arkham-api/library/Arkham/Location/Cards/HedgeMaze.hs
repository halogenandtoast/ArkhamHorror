module Arkham.Location.Cards.HedgeMaze (hedgeMaze) where

import Arkham.Ability
import Arkham.Card.CardDef
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Modifier
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype HedgeMaze = HedgeMaze LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hedgeMaze :: LocationCard HedgeMaze
hedgeMaze = location HedgeMaze Cards.hedgeMaze 2 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.moaiStatues]

instance HasModifiersFor HedgeMaze where
  getModifiersFor (HedgeMaze a) = clearedOfMirages a mirageCards

instance HasAbilities HedgeMaze where
  getAbilities (HedgeMaze a) =
    extendRevealed
      a
      [ mirage a 2 mirageCards
      , restricted a 1 (DuringTurn You)
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ be a) #success
      ]

instance RunMessage HedgeMaze where
  runMessage msg l@(HedgeMaze attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      turnModifiers iid (attrs.ability 1) attrs [ShroudModifier 1]
      turnModifiers iid (attrs.ability 1) iid [CannotMove]
      pure l
    _ -> HedgeMaze <$> mirageRunner Stories.hedgeMaze mirageCards 2 msg attrs
