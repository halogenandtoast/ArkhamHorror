module Arkham.Asset.Cards.PuzzleBox (
  puzzleBox,
  PuzzleBox (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (InvestigatorDefeated)
import Arkham.DamageEffect
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Field
import Arkham.Location.Brazier
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype PuzzleBox = PuzzleBox AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

puzzleBox :: AssetCard PuzzleBox
puzzleBox =
  asset PuzzleBox Cards.puzzleBox

instance HasAbilities PuzzleBox where
  getAbilities (PuzzleBox attrs) =
    [ restrictedAbility attrs 1 (ControlsThis <> InvestigatorExists (NotInvestigator You))
        $ ForcedAbility
        $ InvestigatorDefeated Timing.When ByAny You
    , limitedAbility (GroupLimit PerGame 1)
        $ restrictedAbility
          attrs
          2
          ( ControlsThis
              <> AnyCriterion
                [ enemyExists (enemyIs Enemies.theSpectralWatcher <> NotEnemy ExhaustedEnemy)
                , enemyExists (enemyIs Enemies.theSpectralWatcher <> ExhaustedEnemy) <> CanDealDamage
                , LocationExists (YourLocation <> LocationWithBrazier Lit)
                ]
          )
        $ ActionAbility []
        $ ActionCost 1
    ]

instance RunMessage PuzzleBox where
  runMessage msg a@(PuzzleBox attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      others <- selectList $ NotInvestigator (InvestigatorWithId iid)
      player <- getPlayer iid
      push
        $ chooseOne player [targetLabel other [TakeControlOfAsset other (toId attrs)] | other <- others]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      exhaustedSpectralWatcher <- selectOne $ enemyIs Enemies.theSpectralWatcher <> ExhaustedEnemy
      readySpectralWatcher <- selectOne $ enemyIs Enemies.theSpectralWatcher <> ReadyEnemy
      locationLit <- selectOne $ LocationWithBrazier Lit <> locationWithInvestigator iid
      canDealDamage <- withoutModifier iid CannotDealDamage
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Unlight a brazier at your location"
            [UpdateLocation location (LocationBrazier ?=. Unlit)]
          | location <- maybeToList locationLit
          ]
        <> [ Label "Exhaust the Spectral Watcher" [Exhaust (toTarget enemyId)]
           | enemyId <- maybeToList readySpectralWatcher
           ]
        <> [ Label "Deal 5 damage to the Spectral Watcher" [EnemyDamage enemyId $ nonAttack attrs 5]
           | canDealDamage
           , enemyId <- maybeToList exhaustedSpectralWatcher
           ]
      pure a
    _ -> PuzzleBox <$> runMessage msg attrs
