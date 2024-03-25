module Arkham.Asset.Cards.GuardianAngel (
  guardianAngel,
  GuardianAngel (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GuardianAngel = GuardianAngel AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guardianAngel :: AssetCard GuardianAngel
guardianAngel =
  assetWith GuardianAngel Cards.guardianAngel (healthL ?~ 3)

instance HasModifiersFor GuardianAngel where
  getModifiersFor (InvestigatorTarget iid) (GuardianAngel a)
    | not (controlledBy a iid) = do
        mAssetLocation <- field AssetLocation (toId a)
        valid <- case mAssetLocation of
          Just location ->
            iid
              <=~> InvestigatorAt
                (oneOf [LocationWithId location, ConnectedTo (LocationWithId location)])
          Nothing -> pure False
        pure [toModifier a (CanAssignDamageToAsset $ toId a) | valid]
  getModifiersFor _ _ = pure []

instance HasAbilities GuardianAngel where
  getAbilities (GuardianAngel attrs) =
    [ restrictedAbility attrs 1 ControlsThis $ freeReaction $ AssetDealtDamage #when AnySource (be attrs)
    ]

getDamage :: [Window] -> Int
getDamage ((windowType -> Window.DealtDamage _ _ _ n) : rest) = n + getDamage rest
getDamage (_ : rest) = getDamage rest
getDamage [] = 0

instance RunMessage GuardianAngel where
  runMessage msg a@(GuardianAngel attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows' _ -> do
      let damage = getDamage windows'
      n <- min damage <$> getRemainingBlessTokens
      pushAll $ replicate n $ AddChaosToken BlessToken
      pure a
    _ -> GuardianAngel <$> runMessage msg attrs
