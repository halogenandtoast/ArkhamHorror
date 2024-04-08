module Arkham.Asset.Cards.EldritchSophist (
  eldritchSophist,
  EldritchSophist (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype EldritchSophist = EldritchSophist AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eldritchSophist :: AssetCard EldritchSophist
eldritchSophist = ally EldritchSophist Cards.eldritchSophist (1, 3)

instance HasAbilities EldritchSophist where
  getAbilities (EldritchSophist attrs) =
    [ controlledAbility
        attrs
        1
        ( AnyCriterion
            [ DifferentAssetsExist
                (AssetControlledBy You <> AssetWithUses Charge)
                ( AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
                    <> oneOf [AssetWithUseType Charge, AssetWithoutUses]
                )
            , DifferentAssetsExist
                (AssetControlledBy You <> AssetWithUses Secret)
                ( AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
                    <> oneOf [AssetWithUseType Secret, AssetWithoutUses]
                )
            ]
        )
        $ FastAbility (exhaust attrs)
    ]

instance RunMessage EldritchSophist where
  runMessage msg a@(EldritchSophist attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> oneOf [AssetWithUses Charge, AssetWithUses Secret]

      player <- getPlayer iid
      push
        $ chooseOne
          player
          [targetLabel x [HandleTargetChoice iid (attrs.ability 1) (toTarget x)] | x <- assets]

      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      uses <- field AssetUses aid
      let hasCharge = (> 0) $ findWithDefault 0 Charge uses
      let hasSecret = (> 0) $ findWithDefault 0 Secret uses

      chargeChoices <-
        select
          $ not_ (AssetWithId aid)
          <> AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> oneOf [AssetWithoutUses, AssetWithUseType Charge]
      secretChoices <-
        select
          $ not_ (AssetWithId aid)
          <> AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> oneOf [AssetWithoutUses, AssetWithUseType Secret]

      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [ Label
            "Move Charge"
            [ chooseOne player [targetLabel c [SpendUses (toTarget aid) Charge 1, AddUses c Charge 1]]
            | c <- chargeChoices
            ]
          | hasCharge
          ]
        <> [ Label
            "Move Secret"
            [ chooseOne
                player
                [targetLabel c [SpendUses (toTarget aid) Secret 1, AddUses c Secret 1] | c <- secretChoices]
            ]
           | hasSecret
           ]

      pure a
    _ -> EldritchSophist <$> runMessage msg attrs
