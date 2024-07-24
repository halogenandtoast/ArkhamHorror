module Arkham.Asset.Cards.DarrellsKodak (darrellsKodak, DarrellsKodak (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (DiscoverClues)
import Arkham.Helpers.Ref
import Arkham.Helpers.Window (discoveredClues, discoveredLocation)
import Arkham.Matcher
import Arkham.Token
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype DarrellsKodak = DarrellsKodak AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darrellsKodak :: AssetCard DarrellsKodak
darrellsKodak = asset DarrellsKodak Cards.darrellsKodak

instance HasAbilities DarrellsKodak where
  getAbilities (DarrellsKodak a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (oneOf [EnemySpawns #after Anywhere AnyEnemy, TreacheryEntersPlay #after AnyTreachery])
          (exhaust a)
    , restrictedAbility a 2 ControlsThis
        $ freeReaction
        $ DiscoverClues
          #after
          You
          ( oneOf
              [LocationWithEnemy (EnemyWithToken Evidence), LocationWithTreachery (TreacheryWithToken Evidence)]
          )
          AnyValue
    , restrictedAbility a 2 (ControlsThis <> exists (EnemyWithToken Evidence <> not_ (EnemyAt Anywhere)))
        $ freeReaction
        $ DiscoverClues #after You Anywhere AnyValue
    ]

getKodakTarget :: HasCallStack => [Window] -> Target
getKodakTarget [] = error "Invalid call"
getKodakTarget ((windowType -> Window.EnemySpawns eid _) : _) = EnemyTarget eid
getKodakTarget ((windowType -> Window.TreacheryEntersPlay tid) : _) = TreacheryTarget tid
getKodakTarget (_ : ws) = getKodakTarget ws

instance RunMessage DarrellsKodak where
  runMessage msg a@(DarrellsKodak attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getKodakTarget -> target) _ -> do
      placeTokens (attrs.ability 1) target Evidence 1
      pure a
    UseCardAbility _iid (isSource attrs -> True) 2 (discoveredClues -> n) _ -> do
      push $ DoStep n msg
      pure a
    DoStep n msg'@(UseCardAbility iid (isSource attrs -> True) 2 (discoveredLocation -> lid) _) | n > 0 -> do
      enemies <- selectTargets $ EnemyWithToken Evidence <> oneOf [enemyAt lid, not_ (EnemyAt Anywhere)]
      treacheries <-
        selectTargets $ TreacheryWithToken Evidence <> oneOf [treacheryAt lid, not_ (TreacheryAt Anywhere)]
      chooseOne
        iid
        [ targetLabel
          target
          [ MoveTokens (attrs.ability 2) (targetToSource target) (toTarget attrs) Evidence 1
          , DoStep (n - 1) msg'
          ]
        | target <- enemies <> treacheries
        ]
      pure a
    _ -> DarrellsKodak <$> liftRunMessage msg attrs
