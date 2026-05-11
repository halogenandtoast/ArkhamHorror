module Arkham.EnemyLocation.Cards.ShapelessCellar (shapelessCellar) where

import Arkham.Ability
import Arkham.EnemyLocation.Cards qualified as Cards
import Arkham.EnemyLocation.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher

newtype ShapelessCellar = ShapelessCellar EnemyLocationAttrs
  deriving anyclass IsEnemyLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Massive. Retaliate. Cannot make attacks of opportunity.
-- Shapeless Cellar cannot be sealed or flipped.
-- Health 5. Shroud 4. Reveal clues 3 (PerPlayer? card shows static 3).
shapelessCellar :: EnemyLocationCard ShapelessCellar
shapelessCellar =
  enemyLocationWith
    ShapelessCellar
    Cards.shapelessCellar
    (3, Static 5, 4)
    (2, 1)
    ( \la ->
        la
          { enemyLocationBase =
              (enemyLocationBase la)
                { locationShroud = Just (Static 4)
                , locationRevealClues = Static 3
                , locationWithoutClues = False
                }
          }
    )

instance HasModifiersFor ShapelessCellar where
  getModifiersFor (ShapelessCellar a) = do
    -- Cannot be sealed, cannot be flipped
    modifySelf a [CannotMakeAttacksOfOpportunity, CannotBeFlipped]

instance HasAbilities ShapelessCellar where
  getAbilities (ShapelessCellar a) =
    getAbilities a
      <> [ -- "Forced - After you fail a skill test while investigating Shapeless
           -- Cellar: Shapeless Cellar attacks you."
           mkAbility a 1
             $ forced
             $ SkillTestResult #after You (WhileInvestigating $ LocationWithId a.id) #failure
         , -- "Forced - If there are no clues on Shapeless Cellar: Add it to the
           -- victory display."
           mkAbility a 2
             $ forced
             $ RoundEnds #when
         ]

instance RunMessage ShapelessCellar where
  runMessage msg el@(ShapelessCellar attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push $ Do EnemiesAttack
      pure el
    UseThisAbility _iid (isSource attrs -> True) 2 ->
      pure el
    _ -> ShapelessCellar <$> liftRunMessage msg attrs
