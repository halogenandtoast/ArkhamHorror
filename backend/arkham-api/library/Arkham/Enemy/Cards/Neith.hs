module Arkham.Enemy.Cards.Neith (neith) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (AssetDefeated, InvestigatorDefeated)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype Neith = Neith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

neith :: EnemyCard Neith
neith = enemy Neith Cards.neith (4, PerPlayer 5, 4) (0, 2)

instance HasAbilities Neith where
  getAbilities (Neith a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ oneOf
            [ InvestigatorDefeated
                #after
                (DefeatedByMatches [ByHorror, BySource (SourceIsEnemyAttack $ be a)])
                Anyone
            , AssetDefeated
                #after
                (DefeatedByMatches [ByHorror, BySource (SourceIsEnemyAttack $ be a)])
                (#ally <> AssetControlledBy Anyone)
            ]
      , restricted a 2 (exists $ locationWithEnemy a <> LocationWithDamage (atLeast 1))
          $ forced
          $ EnemyDealtDamage #when AttackDamageEffect (be a) AnySource
      ]

instance RunMessage Neith where
  runMessage msg e@(Neith attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.InvestigatorDefeated _ iid -> investigatorTakenByTheAbyss iid
        Window.AssetDefeated aid _ -> assetTakenByTheAbyss aid
        _ -> pure ()
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      withLocationOf attrs \loc -> do
        removeTokens (attrs.ability 2) loc #damage 1
        reduceDamageTaken (attrs.ability 2) attrs 1
      pure e
    _ -> Neith <$> liftRunMessage msg attrs
