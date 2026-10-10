module Arkham.Homebrew.AgesUnwound.Locations.Paris (paris) where

import Arkham.Ability
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (campaignI18n)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTo)

newtype Paris = Paris LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paris :: LocationCard Paris
paris = symbolLabel $ location Paris Cards.paris 3 (PerPlayer 1)

{- | "[action][action]: __Move__ to Arkham, Massachusetts. / __Forced__ - After
you defeat an enemy in Paris: Lose an action."

The "in Paris" qualifier rides the window ('enemyWasAt'), not a criterion: by
the @#after@ window the enemy is out of play, so a criterion selecting in-play
enemies at this location would find nothing and the Forced would silently never
fire (@project_defeated_enemy_condition_belongs_in_the_window@).
-}
instance HasAbilities Paris where
  getAbilities (Paris a) =
    extendRevealed
      a
      [ campaignI18n
          $ withI18nTooltip "paris.move"
          $ restricted a 1 (Here <> exists (locationIs Cards.arkhamMassachusetts_111))
          $ ActionAbility #move Nothing (ActionCost 2)
      , mkAbility a 2 $ forced $ IfEnemyDefeated #after You ByAny (enemyWasAt a)
      ]

instance RunMessage Paris where
  runMessage msg l@(Paris attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectForMaybeM (locationIs Cards.arkhamMassachusetts_111) $ moveTo (attrs.ability 1) iid
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      loseStandardActions iid (attrs.ability 2) 1
      pure l
    _ -> Paris <$> liftRunMessage msg attrs
