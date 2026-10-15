module Arkham.Homebrew.ThePhantomOfTheOpera.Treacheries.ShroudedFaces (shroudedFaces) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (EnemyEvaded)

newtype ShroudedFaces = ShroudedFaces TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shroudedFaces :: TreacheryCard ShroudedFaces
shroudedFaces = treachery ShroudedFaces Cards.shroudedFaces

instance HasModifiersFor ShroudedFaces where
  getModifiersFor (ShroudedFaces a) = case a.placement of
    AttachedToEnemy eid -> modified_ a eid [EnemyFight 1, HorrorDealt 1]
    _ -> pure mempty

instance HasAbilities ShroudedFaces where
  getAbilities (ShroudedFaces a) = case a.placement of
    AttachedToEnemy eid ->
      [ mkAbility a 1
          $ forced
          $ oneOf
            [ CampaignEvent #after Nothing ("exposed[" <> tshow eid <> "]")
            , EnemyEvaded #after Anyone (EnemyWithId eid)
            ]
      ]
    _ -> []

instance RunMessage ShroudedFaces where
  runMessage msg t@(ShroudedFaces attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      enemies <- select AnyEnemy
      unless (null enemies) do
        chooseOrRunOneM iid $ targets enemies $ attachTreachery attrs
        placeDoom (toSource attrs) attrs 1
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> ShroudedFaces <$> liftRunMessage msg attrs
