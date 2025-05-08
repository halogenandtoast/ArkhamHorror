module Arkham.Asset.Assets.DianneDevineHidingAnOathUnspoken (dianneDevineHidingAnOathUnspoken) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Trait

newtype DianneDevineHidingAnOathUnspoken = DianneDevineHidingAnOathUnspoken AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dianneDevineHidingAnOathUnspoken :: AssetCard DianneDevineHidingAnOathUnspoken
dianneDevineHidingAnOathUnspoken = asset DianneDevineHidingAnOathUnspoken Cards.dianneDevineHidingAnOathUnspoken

instance HasModifiersFor DianneDevineHidingAnOathUnspoken where
  getModifiersFor (DianneDevineHidingAnOathUnspoken a) =
    modifySelect
      a
      (InvestigatorAt $ locationWithAsset a)
      [CannotDiscoverClues, CannotTakeControlOfClues]

instance HasAbilities DianneDevineHidingAnOathUnspoken where
  getAbilities (DianneDevineHidingAnOathUnspoken a) =
    [ restricted
        a
        1
        ( exists
            $ AssetMatches -- avoiding the automatic merge of AssetWithFewestClues
              [ AssetWithFewestClues (withTrait Bystander <> AssetWithClues (atLeast 1))
              , not_ (AssetAt $ locationWithAsset a)
              ]
        )
        $ forced
        $ PhaseBegins #when #enemy
    ]

instance RunMessage DianneDevineHidingAnOathUnspoken where
  runMessage msg e@(DianneDevineHidingAnOathUnspoken attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      bystanders <- select $ AssetWithFewestClues $ withTrait Bystander <> AssetWithClues (atLeast 1)
      leadChooseOneM $ targets bystanders (`withLocationOf` place attrs)
      pure e
    _ -> DianneDevineHidingAnOathUnspoken <$> liftRunMessage msg attrs
