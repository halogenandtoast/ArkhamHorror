module Arkham.Asset.Assets.ThePearlDiaryAGrimAccount (thePearlDiaryAGrimAccount) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGetsWhen)
import Arkham.Helpers.Window (cardDrawnBy)
import Arkham.Matcher
import Arkham.Trait (Trait (Blight, Power))

newtype ThePearlDiaryAGrimAccount = ThePearlDiaryAGrimAccount AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePearlDiaryAGrimAccount :: AssetCard ThePearlDiaryAGrimAccount
thePearlDiaryAGrimAccount = asset ThePearlDiaryAGrimAccount Cards.thePearlDiaryAGrimAccount

instance HasModifiersFor ThePearlDiaryAGrimAccount where
  getModifiersFor (ThePearlDiaryAGrimAccount a) = do
    time <- getCampaignTime
    controllerGetsWhen a (time == Day) [SkillModifier #intellect 1]
    controllerGetsWhen a (time == Night) [SkillModifier #willpower 1]

instance HasAbilities ThePearlDiaryAGrimAccount where
  getAbilities (ThePearlDiaryAGrimAccount a) =
    [ storyControlled_ a 1
        $ triggered
          (DrawCard #after (affectsOthers $ at_ YourLocation) (basic $ CardWithOneOf [CardWithTrait Blight, CardWithTrait Power]) EncounterDeck)
          (exhaust a)
    ]

instance RunMessage ThePearlDiaryAGrimAccount where
  runMessage msg a@(ThePearlDiaryAGrimAccount attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (cardDrawnBy -> (iid', _card)) _ -> do
      drawCards iid' (attrs.ability 1) 1
      pure a
    _ -> ThePearlDiaryAGrimAccount <$> liftRunMessage msg attrs
