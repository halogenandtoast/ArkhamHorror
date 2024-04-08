module Arkham.Asset.Cards.VirgilGrayTrulyInspired (
  virgilGrayTrulyInspired,
  VirgilGrayTrulyInspired (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Capability
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Token

newtype VirgilGrayTrulyInspired = VirgilGrayTrulyInspired AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

virgilGrayTrulyInspired :: AssetCard VirgilGrayTrulyInspired
virgilGrayTrulyInspired = allyWith VirgilGrayTrulyInspired Cards.virgilGrayTrulyInspired (1, 3) (slotsL .~ mempty)

instance HasAbilities VirgilGrayTrulyInspired where
  getAbilities (VirgilGrayTrulyInspired x) =
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
        $ PlacedToken #after AlarmLevel
    , mkAbility x 2
        $ forced
        $ AssetLeavesPlay #when (be x)
    ]

instance RunMessage VirgilGrayTrulyInspired where
  runMessage msg a@(VirgilGrayTrulyInspired attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mDrawCards <- drawCardsIfCan iid (attrs.ability 1) 1
      mGainResources <- gainResourcesIfCan iid (attrs.ability 1) 1
      healable <- selectAny $ HealableAsset (attrs.ability 1) #horror (be attrs)
      others <- select $ not_ (InvestigatorWithId iid)
      player <- getPlayer iid
      push
        $ chooseOne
          player
        $ [ Label "Draw 1 card" [drawing]
          | drawing <- maybeToList mDrawCards
          ]
        <> [ Label "Gain 1 resource" [gainResources]
           | gainResources <- maybeToList mGainResources
           ]
        <> [ Label "Heal 1 horror from Virgil Gray" [HealHorror (toTarget attrs) (attrs.ability 1) 1]
           | healable
           ]
      pushWhen (notNull others)
        $ chooseOrRunOne
          player
          [ targetLabel other [TakeControlOfAsset other attrs.id]
          | other <- others
          ]
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      push $ RemoveFromGame $ toTarget attrs
      pure a
    _ -> VirgilGrayTrulyInspired <$> runMessage msg attrs
