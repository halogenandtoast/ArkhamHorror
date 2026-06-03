module Arkham.EnemyLocation.Cards.ShapelessCellar (shapelessCellar) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype ShapelessCellar = ShapelessCellar EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shapelessCellar :: EnemyLocationCard ShapelessCellar
shapelessCellar =
  enemyLocationWith ShapelessCellar Cards.shapelessCellar (3, PerPlayer 5, 4) (1, 2) \la ->
    la
      { enemyLocationBase =
          (enemyLocationBase la)
            { locationShroud = Just (Static 4)
            , locationRevealClues = PerPlayer 3
            , locationWithoutClues = False
            }
      }

instance HasModifiersFor ShapelessCellar where
  getModifiersFor (ShapelessCellar a) = do
    modifySelf a [CannotMakeAttacksOfOpportunity, CannotBeFlipped]

instance HasAbilities ShapelessCellar where
  getAbilities (ShapelessCellar a) =
    extend
      a
      [ mkAbility a 1
          $ forced
          $ SkillTestResult #after You (WhileInvestigating $ LocationWithId a.id) #failure
      , mkAbility a 2 $ forced $ LastClueRemovedFromLocation #after (LocationWithId a.id)
      ]

instance RunMessage ShapelessCellar where
  runMessage msg el@(ShapelessCellar attrs) = runQueueT $ case msg of
    PlacedLocation _ _ lid | lid == attrs.id -> do
      placeClues attrs attrs =<< perPlayer 3
      pure el
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      sendMessage attrs $ Do EnemiesAttack
      pure el
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      addToVictory_ attrs
      pure el
    _ -> ShapelessCellar <$> liftRunMessage msg attrs
