module Arkham.Event.Cards.DynamiteBlast where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (NonAttackDamageEffect)
import Arkham.Projection

newtype DynamiteBlast = DynamiteBlast EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dynamiteBlast :: EventCard DynamiteBlast
dynamiteBlast = event DynamiteBlast Cards.dynamiteBlast

instance RunMessage DynamiteBlast where
  runMessage msg e@(DynamiteBlast attrs) = case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      currentLocation <- fieldJust InvestigatorLocation iid
      connectedLocations <- selectList $ AccessibleFrom $ LocationWithId currentLocation
      canDealDamage <- withoutModifier iid CannotDealDamage
      choices <- forMaybeM (currentLocation : connectedLocations) $ \location -> do
        enemies <- if canDealDamage then selectList (enemyAt location) else pure []
        investigators <- selectList $ investigatorAt location
        if null enemies && null investigators
          then pure Nothing
          else
            pure
              $ Just
                ( location
                , uiEffect attrs location Explosion
                    : map (nonAttackEnemyDamage attrs 3) enemies
                      <> map (\iid' -> assignDamage iid' attrs 3) investigators
                )
      let availableChoices = map (uncurry targetLabel) $ filter (notNull . snd) choices
      player <- getPlayer iid
      push $ chooseOne player availableChoices
      pure e
    _ -> DynamiteBlast <$> runMessage msg attrs
