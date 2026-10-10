module Arkham.Homebrew.AgesUnwound.Treacheries.TheColoursSpread (theColoursSpread) where

import Arkham.Ability
import Arkham.Asset.Types qualified as Field
import Arkham.DefeatedBy (defeatedBySource)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Window (getDefeatedAsset)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Projection
import Arkham.Strategy
import Arkham.Trait (Trait (Ally))
import Arkham.Treachery.Import.Lifted hiding (AssetDefeated)
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype TheColoursSpread = TheColoursSpread TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Put into play next to the agenda deck by Scenario VI's setup if /the Myriad
took control of a colour out of space/, and removed from the game otherwise.
No 'Revelation' for the same reason as /Nexus of Aforgomon/.
-}
theColoursSpread :: TreacheryCard TheColoursSpread
theColoursSpread = treachery TheColoursSpread Cards.theColoursSpread

-- | "a copy of The Myriad Gentleman" -- four printings share that title, so this
-- is matched by title rather than by def.
theMyriadGentleman :: EnemyMatcher
theMyriadGentleman = EnemyWithTitle "The Myriad Gentleman"

{- | "The Colour's Spread cannot leave play. /
Forced - When a copy of The Myriad Gentleman attacks you: Damage and horror from
this attack must be assigned to your [[Ally]] assets, if possible."

The assignment half is a static 'SetAttackDamageStrategy' on every Myriad
Gentleman rather than something the Forced applies: the printed Forced offers no
choice, and @Do (EnemyAttack …)@ reads the strategy off the enemy's modifiers
/before/ the @when … attacks@ window resolves (@Enemy/Runner.hs:1590@), so a
modifier applied from inside that window would arrive too late.
'DamageAndHorrorAssetsFirst' is the engine's own both-halves-at-once strategy,
and its matcher is already scoped to the damaged investigator's own assets
(the shape /Abhorrent Moon-Beast/ and /Enraged Gug/ use).
-}
instance HasModifiersFor TheColoursSpread where
  getModifiersFor (TheColoursSpread a) = do
    modifySelf a [CannotLeavePlay]
    modifySelect
      a
      theMyriadGentleman
      [SetAttackDamageStrategy $ DamageAndHorrorAssetsFirst (AssetWithTrait Ally)]

{- | "Place any [[Ally]] assets defeated by this attack beneath the attacking
enemy, as swarm cards."

@#when@, not @#after@: the window's own 'AssetMatcher' is matched by
@select@ over assets /in play/, and at @#after@ the asset is already gone.
-}
instance HasAbilities TheColoursSpread where
  getAbilities (TheColoursSpread a) =
    [ mkAbility a 1
        $ forced
        $ AssetDefeated #when (BySource $ SourceIsEnemyAttack theMyriadGentleman)
        $ AssetWithTrait Ally
    ]

-- | The enemy whose attack defeated the asset, taken off the window rather than
-- re-selected: two Myriad Gentlemen can be in play at once.
attackingEnemy :: [Window] -> Maybe EnemyId
attackingEnemy = \case
  ((windowType -> Window.AssetDefeated _ defeatedBy) : _) -> (defeatedBySource defeatedBy).enemy
  (_ : rest) -> attackingEnemy rest
  [] -> Nothing

instance RunMessage TheColoursSpread where
  runMessage msg t@(TheColoursSpread attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      let aid = getDefeatedAsset ws
      for_ (attackingEnemy ws) \eid -> do
        card <- field Field.AssetCard aid
        {- The asset leaves play as a card rather than being discarded: the
        defeat's own discard is still queued behind this, and 'RemoveAsset'
        makes it a no-op. Same pair as War of the Outer Gods' @placeAssetAsSwarm@. -}
        push $ RemoveAsset aid
        push $ PlacedSwarmCard eid card
      pure t
    _ -> TheColoursSpread <$> liftRunMessage msg attrs
