module Arkham.Enemy.Cards.HorrifyingShade (horrifyingShade) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (DiscoverClues)
import Arkham.Helpers.Modifiers
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HorrifyingShade = HorrifyingShade EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrifyingShade :: EnemyCard HorrifyingShade
horrifyingShade = enemy HorrifyingShade Cards.horrifyingShade (3, Static 3, 3) (1, 1)

instance HasModifiersFor HorrifyingShade where
  getModifiersFor (HorrifyingShade a) = do
    modifySelfWhenM
      a
      (selectAny $ locationWithEnemy a <> LocationClearedOfMirages)
      [RemoveKeyword Aloof]

instance HasAbilities HorrifyingShade where
  getAbilities (HorrifyingShade a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ oneOf
            [ TakeControlOfClues #after You (SourceIsLocation $ locationWithEnemy a)
            , DiscoverClues #after You (locationWithEnemy a) (atLeast 1)
            ]
      ]

getClues :: [Window] -> Int
getClues = sum . map (clueCount . windowType)
 where
  clueCount = \case
    Window.TakeControlOfClues _ _ n -> n
    Window.DiscoverClues _ _ _ n -> n
    _ -> 0

instance RunMessage HorrifyingShade where
  runMessage msg e@(HorrifyingShade attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getClues -> n) _ -> do
      placeDoom (attrs.ability 1) attrs n
      pure e
    _ -> HorrifyingShade <$> liftRunMessage msg attrs
