module Arkham.Asset.Assets.StHubertsKey (stHubertsKey, StHubertsKey (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Classes.HasQueue (findFromQueue)
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Matcher
import Arkham.Message (MessageType (..), pattern CancelNext)
import Arkham.Message qualified as Msg

newtype StHubertsKey = StHubertsKey AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stHubertsKey :: AssetCard StHubertsKey
stHubertsKey = asset StHubertsKey Cards.stHubertsKey

instance HasModifiersFor StHubertsKey where
  getModifiersFor (StHubertsKey a) = controllerGets a [SkillModifier #willpower 1, SkillModifier #intellect 1, SanityModifier (-2)]

instance HasAbilities StHubertsKey where
  getAbilities (StHubertsKey a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InvestigatorDefeated #when ByHorror $ HealableInvestigator (toSource a) #horror You)
        $ DiscardCost FromPlay
        $ toTarget a
    ]

instance RunMessage StHubertsKey where
  runMessage msg a@(StHubertsKey attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      mDefeatedMessage <- lift $ findFromQueue \case
        Msg.InvestigatorIsDefeated {} -> True
        Msg.InvestigatorDefeated {} -> True
        _ -> False
      let
        defeatedSource = case mDefeatedMessage of
          Just (Msg.InvestigatorDefeated x _) -> x
          Just (Msg.InvestigatorIsDefeated x _) -> x
          _ -> error "missing defeated message"
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      when canHeal $ healHorror iid (attrs.ability 1) 2
      push $ CancelNext (toSource attrs) InvestigatorDefeatedMessage
      checkDefeated defeatedSource iid
      pure a
    _ -> StHubertsKey <$> liftRunMessage msg attrs
