module Arkham.Campaigns.TheScarletKeys.Key.Cards.TheLastBlossom (theLastBlossom) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Cards
import Arkham.Campaigns.TheScarletKeys.Key.Import.Lifted
import Arkham.Helpers.Investigator (canHaveDamageHealed, canHaveHorrorHealed)
import Arkham.Matcher hiding (key)
import Control.Monad.State.Strict (execStateT, put)

newtype TheLastBlossom = TheLastBlossom ScarletKeyAttrs
  deriving anyclass (IsScarletKey, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLastBlossom :: ScarletKeyCard TheLastBlossom
theLastBlossom = key TheLastBlossom Cards.theLastBlossom

instance HasAbilities TheLastBlossom where
  getAbilities (TheLastBlossom a) = case a.bearer of
    InvestigatorTarget iid | not a.shifted ->
      if a.stable
        then
          [ restricted
              a
              1
              ( exists
                  ( oneOf
                      [HealableInvestigator (a.ability 1) k (affectsOthers $ colocatedWith iid) | k <- [#damage, #horror]]
                  )
              )
              $ FastAbility Free
          ]
        else [restricted a 1 (exists EnemyWithAnyDamage) $ FastAbility Free]
    _ -> []

instance RunMessage TheLastBlossom where
  runMessage msg k@(TheLastBlossom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> liftRunMessage (CampaignSpecific "shift[09544]" Null) k
    CampaignSpecific "shift[09544]" _ -> do
      shiftKey attrs do
        when attrs.unstable do
          enemies <- select $ InPlayEnemy EnemyWithAnyDamage
          for_ enemies $ healDamageOn attrs 1
          unless (null enemies) $ withInvestigatorBearer attrs (`flipOver` attrs)
        when attrs.stable do
          withInvestigatorBearer attrs \iid -> do
            willHeal <- flip execStateT False do
              selectEach (affectsOthers $ colocatedWith iid) \iid' -> do
                whenM (canHaveDamageHealed attrs iid') do
                  put True
                  healDamage iid attrs 1
                whenM (canHaveHorrorHealed attrs iid') do
                  put True
                  healHorror iid attrs 1
            when willHeal $ flipOver iid attrs

      pure k
    _ -> TheLastBlossom <$> liftRunMessage msg attrs
