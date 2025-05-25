module Arkham.Event.Events.EatLead2 (eatLead2) where

import Arkham.Asset.Types (Field (AssetUses))
import Arkham.Asset.Uses
import Arkham.ChaosBag.RevealStrategy
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Modifier
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Metadata = Metadata {asset :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EatLead2 = EatLead2 (EventAttrs `With` Metadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eatLead2 :: EventCard EatLead2
eatLead2 = event (EatLead2 . (`With` Metadata Nothing)) Cards.eatLead2

instance RunMessage EatLead2 where
  runMessage msg e@(EatLead2 (attrs `With` metadata)) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ [windowType -> Window.ActivateAbility _ _ ability] _ | eid == toId attrs -> do
      case ability.source.asset of
        Just aid -> do
          uses <- fieldMap AssetUses (findWithDefault 0 Ammo) aid
          chooseAmounts iid "Additional ammo to spend" (MaxAmountTarget uses) [("Ammo", (0, uses))] attrs
          pure . EatLead2 $ attrs `with` Metadata (Just aid)
        _ -> error "Invalid source"
    ResolveAmounts iid (getChoiceAmount "Ammo" -> ammo) target | isTarget attrs target -> do
      let aid = fromJustNote "asset must be set" (asset metadata)
      when (ammo > 0) $ do
        withSkillTest \sid -> do
          push $ SpendUses (toSource attrs) (AssetTarget aid) Ammo ammo
          skillTestModifier sid attrs iid (ChangeRevealStrategy $ RevealAndChoose ammo 1)
          checkAfter $ Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs
      pure e
    _ -> EatLead2 . (`with` metadata) <$> liftRunMessage msg attrs
