module Arkham.Scenarios.FateOfTheVale.CosmicEmissary where

import Arkham.Ability
import Arkham.Classes.HasQueue (push)
import Arkham.Enemy.Types
import Arkham.Id
import Arkham.Matcher
import Arkham.Message hiding (EnemyEvaded, RevealChaosToken)
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Window (Window, getBatchId)

cosmicEmissaryColourAbilities :: EnemyAttrs -> [Ability]
cosmicEmissaryColourAbilities attrs =
  [ forcedAbility attrs 1 $ oneOf [EnemyAttackedSuccessfully #when Anyone AnySource (be attrs), EnemyEvadedSuccessfully #when Anyone AnySource (be attrs)
  ]]

handleCosmicEmissaryColour :: ReverseQueue m => EnemyAttrs -> [Window] -> m ()
handleCosmicEmissaryColour attrs (getBatchId -> batchId) = do
  push $ IgnoreBatch batchId
  roundModifier (attrs.ability 1) attrs CannotAttack

cosmicEmissaryShatteredAbility :: EnemyAttrs -> ChaosTokenMatcher -> [Ability]
cosmicEmissaryShatteredAbility attrs token =
  [ groupLimit PerTest
      $ restricted
        attrs
        1
        (DuringSkillTest (at_ $ locationWithEnemy attrs.id) <> thisIs attrs (EnemyCanAttack You))
      $ forced
      $ RevealChaosToken #after You token
  ]

resolveCosmicEmissaryShatteredAbility :: ReverseQueue m => EnemyAttrs -> InvestigatorId -> m ()
resolveCosmicEmissaryShatteredAbility attrs iid = do
  chooseOneM iid do
    labeledI "automaticallyFailTest" failSkillTest
    labeled "This enemy attacks you" $ initiateEnemyAttack attrs (attrs.ability 1) iid
