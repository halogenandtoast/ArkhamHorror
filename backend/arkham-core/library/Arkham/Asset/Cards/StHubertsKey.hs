module Arkham.Asset.Cards.StHubertsKey (
  stHubertsKey,
  StHubertsKey (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (InvestigatorDefeated)
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype StHubertsKey = StHubertsKey AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stHubertsKey :: AssetCard StHubertsKey
stHubertsKey = asset StHubertsKey Cards.stHubertsKey

instance HasModifiersFor StHubertsKey where
  getModifiersFor (InvestigatorTarget iid) (StHubertsKey a) | controlledBy a iid = do
    pure
      $ toModifiers a [SkillModifier #willpower 1, SkillModifier #intellect 1, SanityModifier (-2)]
  getModifiersFor _ _ = pure []

instance HasAbilities StHubertsKey where
  getAbilities (StHubertsKey a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InvestigatorDefeated #when ByHorror $ HealableInvestigator (toSource a) #horror You)
        $ DiscardCost FromPlay
        $ toTarget a
    ]

instance RunMessage StHubertsKey where
  runMessage msg a@(StHubertsKey attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      mDefeatedMessage <- findFromQueue \case
        Msg.InvestigatorDefeated {} -> True
        _ -> False
      let
        defeatedSource = case mDefeatedMessage of
          Just (Msg.InvestigatorDefeated x _) -> x
          _ -> error "missing defeated message"
      mHealHorror <- getHealHorrorMessage attrs 2 iid
      pushAll
        $ maybeToList mHealHorror
        <> [ CancelNext (toSource attrs) InvestigatorDefeatedMessage
           , checkDefeated defeatedSource iid
           ]
      pure a
    _ -> StHubertsKey <$> runMessage msg attrs
