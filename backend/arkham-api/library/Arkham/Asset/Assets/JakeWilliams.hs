module Arkham.Asset.Assets.JakeWilliams (jakeWilliams, JakeWilliams (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype JakeWilliams = JakeWilliams AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jakeWilliams :: AssetCard JakeWilliams
jakeWilliams = ally JakeWilliams Cards.jakeWilliams (3, 2)

instance HasModifiersFor JakeWilliams where
  getModifiersFor (JakeWilliams a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> do
      actions <- fieldMap InvestigatorActionsTaken concat iid
      modified_ a iid $ do
        action <- [#move, #investigate]
        guard $ action `notElem` actions
        pure $ ActionDoesNotCauseAttacksOfOpportunity action

instance HasAbilities JakeWilliams where
  getAbilities (JakeWilliams a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility
          ( oneOf
              [ RevealLocation #after You Anywhere
              , PutLocationIntoPlay #after You Anywhere
              ]
          )
          (exhaust a)
    ]

instance RunMessage JakeWilliams where
  runMessage msg a@(JakeWilliams attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure a
    _ -> JakeWilliams <$> liftRunMessage msg attrs
