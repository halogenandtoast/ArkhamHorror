module Arkham.Story.Cards.SaveTheCivilians (saveTheCivilians) where

import Arkham.Ability
import Arkham.Card.CardCode
import Arkham.Helpers.Location
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Token
import Arkham.Window qualified as Window

newtype SaveTheCivilians = SaveTheCivilians StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saveTheCivilians :: StoryCard SaveTheCivilians
saveTheCivilians = persistStory $ story SaveTheCivilians Cards.saveTheCivilians

instance HasAbilities SaveTheCivilians where
  getAbilities (SaveTheCivilians a) =
    [ restricted
        a
        1
        (exists $ YourLocation <> LocationWithToken Civilian)
        (actionAbilityWithCost $ clueCost 1)
    ]

instance RunMessage SaveTheCivilians where
  runMessage msg s@(SaveTheCivilians attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      pure s
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        removeTokens (attrs.ability 1) lid Civilian 1
        placeTokens (attrs.ability 1) attrs.id Civilian 1
        cardCode <- field LocationCardCode lid
        checkAfter $ Window.ScenarioEvent ("rescueCivilian[" <> unCardCode cardCode <> "]") (Just iid) Null
      pure s
    _ -> SaveTheCivilians <$> liftRunMessage msg attrs
