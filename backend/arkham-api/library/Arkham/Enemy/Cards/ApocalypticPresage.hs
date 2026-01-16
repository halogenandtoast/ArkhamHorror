module Arkham.Enemy.Cards.ApocalypticPresage (apocalypticPresage) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers (hollow)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ApocalypticPresage = ApocalypticPresage EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apocalypticPresage :: EnemyCard ApocalypticPresage
apocalypticPresage = enemy ApocalypticPresage Cards.apocalypticPresage (4, Static 5, 2) (1, 2)

instance HasAbilities ApocalypticPresage where
  getAbilities (ApocalypticPresage a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemySpawns #when Anywhere (be a)
      , mkAbility a 2 $ freeReaction $ EnemyDefeated #when Anyone ByAny (be a)
      ]

instance RunMessage ApocalypticPresage where
  runMessage msg e@(ApocalypticPresage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        cards <-
          select
            $ basic (NonWeakness <> not_ PermanentCard)
            <> oneOf [inHandOf NotForPlay iid, inPlayAreaOf iid]
        focusCards cards $ chooseTargetM iid cards $ hollow iid
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      investigators <- getInvestigators
      hollowed <- for investigators \iid -> do
        mods <- getModifiers iid
        hollows <- traverse fetchCard [cardId | Hollow cardId <- mods]
        pure (iid, hollows)
      lead <- getLead
      focusCards (concatMap snd hollowed) do
        chooseUpToNM_ lead 3 do
          for_ hollowed \(iid, cards) -> do
            for_ cards \card -> targeting card $ addToHand iid [card]
      defeatModifier attrs.id (attrs.ability 1) attrs LoseVictory
      pure e
    _ -> ApocalypticPresage <$> liftRunMessage msg attrs
