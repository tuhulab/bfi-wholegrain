# MATLAB Setup Guide

This document provides setup instructions for running the MATLAB components of the bfi-wholegrain project.

## Prerequisites

### MATLAB Version
- **MATLAB R2016a or later** is recommended
- Tested with MATLAB R2016a-R2020b

### Required Toolboxes
The following MATLAB toolboxes are required:

1. **Statistics and Machine Learning Toolbox**
   - Used for: PLS-DA, cross-validation, statistical tests
   - Functions: `plsregress`, `crossval`, `cvpartition`

2. **Bioinformatics Toolbox** 
   - Used for: Data preprocessing and manipulation
   - Functions: Various bioinformatics utilities

### Optional Toolboxes
These toolboxes enhance functionality but are not strictly required:

- **Optimization Toolbox**: For advanced optimization algorithms
- **Parallel Computing Toolbox**: For faster processing of large datasets

## Installation

### 1. Clone Repository
```bash
git clone https://github.com/tuhulab/bfi-wholegrain.git
cd bfi-wholegrain
```

### 2. Verify MATLAB Installation
Open MATLAB and run:

```matlab
% Check MATLAB version
ver

% Check installed toolboxes
ver('stats')
ver('bioinfo')
```

### 3. Add Project to MATLAB Path
```matlab
% In MATLAB, navigate to the project directory
cd /path/to/bfi-wholegrain

% Add matlab folder to path
addpath(genpath('matlab'));
savepath;
```

## Directory Structure

```
matlab/
├── Arrange_Data_MZmine2.m       % Main data arrangement from MZmine2
├── Arrange_Data_MZmine2_blank.m % Blank sample handling
├── arrange_serum_pos.m          % Serum positive mode processing
├── arrange_serum_neg.m          % Serum negative mode processing
├── arrange_urine_pos.m          % Urine positive mode processing
├── varselcv.m                   % Variable selection with CV
├── varsel_test.m                % Variable selection testing
├── group_markers.m              % Group potential markers
├── deisotope.m                  % Remove isotope peaks
├── dubremove_wo_corr.m          % Remove duplicates
├── find_feature.m               % Feature identification
├── findfree.m                   % Find free metabolites
├── exfeatures.m                 % Extract features
├── clean_data.m                 % Data cleaning
├── arrangeblanksample.m         % Arrange blank samples
├── arrangedatapos.m             % Arrange positive mode data
├── arrangedataneg.m             % Arrange negative mode data
├── arrangevarsel.m              % Arrange variable selection results
└── sep_testcal.m                % Separate test and calibration sets
```

## Usage Examples

### Example 1: Process Serum Data (Positive Mode)

```matlab
% Navigate to matlab directory
cd matlab

% Set parameters
mode = 'pos';
project = 'M226_barley';
tissue = 'serum';

% Run preprocessing
arrange_serum_pos

% Perform variable selection
varselcv
```

### Example 2: Arrange MZmine2 Output

```matlab
% Load MZmine2 output
data_file = 'path/to/mzmine2_output.csv';

% Process with blank subtraction
Arrange_Data_MZmine2_blank

% Clean and organize data
clean_data
```

### Example 3: Variable Selection

```matlab
% Load preprocessed data
load('serum_pos.mat');

% Configure parameters
ncomp = 5;        % Number of PLS components
nfold = 7;        % Number of CV folds
nrep = 100;       % Number of repetitions

% Run variable selection with cross-validation
[selected_vars, scores] = varselcv(data, class, ncomp, nfold, nrep);

% Visualize results
figure;
bar(scores);
xlabel('Variable Index');
ylabel('Importance Score');
title('Variable Importance in PLS-DA Model');
```

## Data Format Requirements

### Input Data Structure
MATLAB scripts expect data in specific formats:

#### 1. MZmine2 CSV Output
```
m/z, RT, Sample1, Sample2, ..., SampleN
123.456, 1.23, 1000, 1200, ..., 900
234.567, 2.34, 2000, 2300, ..., 1800
```

#### 2. Sample List (Excel)
Required columns:
- `sample`: Sample identifier
- `class`: Class/group label
- `batch`: Batch number
- `injection_order`: Injection sequence

#### 3. MATLAB Data Structure
```matlab
data.X          % n × p matrix of metabolite intensities
data.label      % Sample labels/metadata
data.class      % Class assignments
data.mz         % m/z values
data.rt         % Retention times
```

## Common Issues and Solutions

### Issue 1: Missing Toolbox
**Error**: `Undefined function or variable 'plsregress'`

**Solution**: Install Statistics and Machine Learning Toolbox
```matlab
% Check if installed
license('test', 'statistics_toolbox')
% If returns 0, install via MATLAB Add-Ons
```

### Issue 2: Path Not Set
**Error**: `Undefined function 'varselcv'`

**Solution**: Add matlab folder to path
```matlab
addpath(genpath('/path/to/bfi-wholegrain/matlab'));
```

### Issue 3: Memory Errors
**Error**: `Out of memory`

**Solution**: Process data in batches or increase MATLAB memory
```matlab
% Increase Java heap memory in preferences
% Or process subset of samples
```

## Performance Optimization

### Parallel Processing
If Parallel Computing Toolbox is available:

```matlab
% Start parallel pool
parpool(4);  % Use 4 workers

% Your processing code here
% Many functions automatically use parallel pool

% Close pool when done
delete(gcp);
```

### Memory Management
```matlab
% Clear unused variables
clear unused_var;

% Pack memory
pack;

% Monitor memory usage
whos;
memory;
```

## Testing Installation

Run this test script to verify setup:

```matlab
%% Test MATLAB Setup for bfi-wholegrain

% Test 1: Check MATLAB version
v = ver('MATLAB');
fprintf('MATLAB Version: %s\\n', v.Release);

% Test 2: Check required toolboxes
toolboxes = {'Statistics and Machine Learning Toolbox', ...
             'Bioinformatics Toolbox'};
for i = 1:length(toolboxes)
    if license('test', lower(strrep(toolboxes{i}, ' ', '_')))
        fprintf('✓ %s: Installed\\n', toolboxes{i});
    else
        fprintf('✗ %s: NOT INSTALLED\\n', toolboxes{i});
    end
end

% Test 3: Check if functions are accessible
functions = {'Arrange_Data_MZmine2', 'varselcv', 'deisotope'};
for i = 1:length(functions)
    if exist(functions{i}, 'file')
        fprintf('✓ Function %s: Found\\n', functions{i});
    else
        fprintf('✗ Function %s: NOT FOUND (check path)\\n', functions{i});
    end
end

fprintf('\\nSetup verification complete.\\n');
```

## Additional Resources

- [MATLAB Statistics Toolbox Documentation](https://www.mathworks.com/help/stats/)
- [MATLAB Bioinformatics Toolbox Documentation](https://www.mathworks.com/help/bioinfo/)
- [PLS-DA Tutorial](https://www.mathworks.com/help/stats/partial-least-squares-regression-and-principal-components-regression.html)

## Support

For issues specific to this project:
- Open an issue on GitHub
- Check existing issues and documentation

For MATLAB-specific issues:
- [MATLAB Answers](https://www.mathworks.com/matlabcentral/answers/)
- [MathWorks Support](https://www.mathworks.com/support.html)
