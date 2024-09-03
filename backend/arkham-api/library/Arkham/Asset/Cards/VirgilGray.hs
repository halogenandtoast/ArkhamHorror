module Arkham.Asset.Cards.VirgilGray (virgilGray, VirgilGray (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (chooseOne, chooseOrRunOne)
import Arkham.Capability
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.ScenarioLogKey

newtype VirgilGray = VirgilGray AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

virgilGray :: AssetCard VirgilGray
virgilGray = allyWith VirgilGray Cards.virgilGray (1, 3) (slotsL .~ mempty)

instance HasAbilities VirgilGray where
  getAbilities (VirgilGray x) =
    [ controlledAbility
        x
        1
        ( oneOf
            [ youExist can.gain.resources
            , youExist can.draw.cards
            , exists $ HealableAsset (x.ability 1) #horror (be x)
            ]
        )
        $ freeReaction
        $ ScenarioCountIncremented #after SignOfTheGods
    , mkAbility x 2
        $ forced
        $ AssetLeavesPlay #when (be x)
    ]

instance RunMessage VirgilGray where
  runMessage msg a@(VirgilGray attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mDrawCards <- Msg.drawCardsIfCan iid (attrs.ability 1) 1
      mGainResources <- Msg.gainResourcesIfCan iid (attrs.ability 1) 1
      healable <- selectAny $ HealableAsset (attrs.ability 1) #horror (be attrs)
      others <- select $ not_ (InvestigatorWithId iid)
      chooseOne
        iid
        $ [ Label "Draw 1 card" [drawing]
          | drawing <- maybeToList mDrawCards
          ]
        <> [ Label "Gain 1 resource" [gainResources]
           | gainResources <- maybeToList mGainResources
           ]
        <> [ Label "Heal 1 horror from Virgil Gray" [HealHorror (toTarget attrs) (attrs.ability 1) 1]
           | healable
           ]
      when (notNull others)
        $ chooseOrRunOne
          iid
          [ targetLabel other [TakeControlOfAsset other attrs.id]
          | other <- others
          ]
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ RemoveFromGame $ toTarget attrs
      pure a
    _ -> VirgilGray <$> liftRunMessage msg attrs
