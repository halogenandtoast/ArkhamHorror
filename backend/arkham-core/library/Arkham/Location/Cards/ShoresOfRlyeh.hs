module Arkham.Location.Cards.ShoresOfRlyeh
  ( shoresOfRlyeh
  , ShoresOfRlyeh(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types ( Field (..) )
import Arkham.Enemy.Types ( Field (..) )
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Types ( Field (..) )

newtype ShoresOfRlyeh = ShoresOfRlyeh LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shoresOfRlyeh :: LocationCard ShoresOfRlyeh
shoresOfRlyeh = location ShoresOfRlyeh Cards.shoresOfRlyeh 1 (Static 2)

instance HasModifiersFor ShoresOfRlyeh where
  getModifiersFor target (ShoresOfRlyeh a) | isTarget a target = do
    enemyDoom <- selectAgg Sum EnemyDoom $ enemyAt (toId a)
    treacheryDoom <- selectAgg Sum TreacheryDoom $ treacheryAt (toId a)
    assetDoom <- selectAgg Sum AssetDoom $ assetAt (toId a)
    investigatorDoom <- selectAgg Sum InvestigatorDoom $ investigatorAt (toId a)
    doomOnSelf <- fieldMap LocationDoom Sum (toId a)
    pure $ toModifiers
      a
      [ ShroudModifier $ getSum $ fold
          [ enemyDoom
          , treacheryDoom
          , assetDoom
          , investigatorDoom
          , doomOnSelf
          ]
      ]
  getModifiersFor _ _ = pure []


instance HasAbilities ShoresOfRlyeh where
  getAbilities (ShoresOfRlyeh a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ PutLocationIntoPlay Timing.After Anyone
      $ LocationWithId
      $ toId a
    ]

instance RunMessage ShoresOfRlyeh where
  runMessage msg l@(ShoresOfRlyeh attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      push $ PlaceDoom (toTarget attrs) 1
      pure l
    _ -> ShoresOfRlyeh <$> runMessage msg attrs
