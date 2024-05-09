import numpy as np
import xarray as xr
import re
from pathlib import Path


def distance(val, ref):
    return abs(ref - val)


vectDistance = np.vectorize(distance)


def cmap_xmap(function, cmap):
    """ Applies function, on the indices of colormap cmap. Beware, function
    should map the [0, 1] segment to itself, or you are in for surprises.

    See also cmap_xmap.
    """
    cdict = cmap._segmentdata
    function_to_map = lambda x: (function(x[0]), x[1], x[2])
    for key in ('red', 'green', 'blue'):
        cdict[key] = map(function_to_map, cdict[key])
    #        cdict[key].sort()
    #        assert (cdict[key][0]<0 or cdict[key][-1]>1), "Resulting indices extend out of the [0, 1] segment."
    return matplotlib.colors.LinearSegmentedColormap('colormap', cdict, 1024)


def getClosest(sortedMatrix, column, val):
    while len(sortedMatrix) > 3:
        half = int(len(sortedMatrix) / 2)
        sortedMatrix = sortedMatrix[-half - 1:] if sortedMatrix[half, column] < val else sortedMatrix[: half + 1]
    if len(sortedMatrix) == 1:
        result = sortedMatrix[0].copy()
        result[column] = val
        return result
    else:
        safecopy = sortedMatrix.copy()
        safecopy[:, column] = vectDistance(safecopy[:, column], val)
        minidx = np.argmin(safecopy[:, column])
        safecopy = safecopy[minidx, :].A1
        safecopy[column] = val
        return safecopy


def convert(column, samples, matrix):
    return np.matrix([getClosest(matrix, column, t) for t in samples])


def valueOrEmptySet(k, d):
    return (d[k] if isinstance(d[k], set) else {d[k]}) if k in d else set()


def mergeDicts(d1, d2):
    """
    Creates a new dictionary whose keys are the union of the keys of two
    dictionaries, and whose values are the union of values.

    Parameters
    ----------
    d1: dict
        dictionary whose values are sets
    d2: dict
        dictionary whose values are sets

    Returns
    -------
    dict
        A dict whose keys are the union of the keys of two dictionaries,
    and whose values are the union of values

    """
    res = {}
    for k in d1.keys() | d2.keys():
        res[k] = valueOrEmptySet(k, d1) | valueOrEmptySet(k, d2)
    return res


def extractCoordinates(filename):
    """
    Scans the header of an Alchemist file in search of the variables.

    Parameters
    ----------
    filename : str
        path to the target file
    mergewith : dict
        a dictionary whose dimensions will be merged with the returned one

    Returns
    -------
    dict
        A dictionary whose keys are strings (coordinate name) and values are
        lists (set of variable values)

    """
    with open(filename, 'r') as file:
        # regex = re.compile(' (?P<varName>[a-zA-Z._-]+) = (?P<varValue>[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?),?')
        regex = r"(?P<varName>[a-zA-Z._-]+) = (?P<varValue>[^,]*),?"
        dataBegin = r"\d"
        is_float = r"[-+]?\d*\.?\d+(?:[eE][-+]?\d+)?"
        for line in file:
            match = re.findall(regex, line.replace('Infinity', '1e30000'))
            if match:
                return {
                    var: float(value) if re.match(is_float, value)
                    else bool(re.match(r".*?true.*?", value.lower())) if re.match(r".*?(true|false).*?", value.lower())
                    else value.replace('\n', '')
                    for var, value in match
                }
            elif re.match(dataBegin, line[0]):
                return {}


def extractVariableNames(filename):
    """
    Gets the variable names from the Alchemist data files header.

    Parameters
    ----------
    filename : str
        path to the target file

    Returns
    -------
    list of list
        A matrix with the values of the csv file

    """
    with open(filename, 'r') as file:
        dataBegin = re.compile('\d')
        lastHeaderLine = ''
        for line in file:
            if dataBegin.match(line[0]):
                break
            else:
                lastHeaderLine = line
        if lastHeaderLine:
            regex = re.compile(' (?P<varName>\S+)')
            return regex.findall(lastHeaderLine)
        return []


def openCsv(path):
    """
    Converts an Alchemist export file into a list of lists representing the matrix of values.

    Parameters
    ----------
    path : str
        path to the target file

    Returns
    -------
    list of list
        A matrix with the values of the csv file

    """
    regex = re.compile('\d')
    with open(path, 'r') as file:
        lines = filter(lambda x: regex.match(x[0]), file.readlines())
        return [[float(x) for x in line.split()] for line in lines]


def beautifyValue(v):
    """
    Converts an object to a better version for printing, in particular:
        - if the object converts to float, then its float value is used
        - if the object can be rounded to int, then the int value is preferred

    Parameters
    ----------
    v : object
        the object to try to beautify

    Returns
    -------
    object or float or int
        the beautified value
    """
    try:
        v = float(v)
        if v.is_integer():
            return int(v)
        return v
    except:
        return v


if __name__ == '__main__':
    # CONFIGURE SCRIPT
    # Where to find Alchemist data files
    directory = 'data'
    # Where to save charts
    output_directory = 'charts'
    # How to name the summary of the processed data
    pickleOutput = 'data_summary'
    # Experiment prefixes: one per experiment (root of the file name)
    experiments = ['stable-gradient', 'stable-scr', 'rescue']
    floatPrecision = '{: 0.3f}'
    # Number of time samples 
    timeSamples = 1000
    # time management
    minTime = 50
    maxTime = 1100
    timeColumnName = 'time'
    logarithmicTime = False
    # One or more variables are considered random and "flattened"
    seedVars = ['seed']


    # Label mapping
    class Measure:
        def __init__(self, description, unit=None):
            self.__description = description
            self.__unit = unit

        def description(self):
            return self.__description

        def unit(self):
            return '' if self.__unit is None else f'({self.__unit})'

        def derivative(self, new_description=None, new_unit=None):
            def cleanMathMode(s):
                return s[1:-1] if s[0] == '$' and s[-1] == '$' else s

            def deriveString(s):
                return r'$d ' + cleanMathMode(s) + r'/{dt}$'

            def deriveUnit(s):
                return f'${cleanMathMode(s)}' + '/{s}$' if s else None

            result = Measure(
                new_description if new_description else deriveString(self.__description),
                new_unit if new_unit else deriveUnit(self.__unit),
            )
            return result

        def __str__(self):
            return f'{self.description()} {self.unit()}'


    centrality_label = 'H_a(x)'


    def expected(x):
        return r'\mathbf{E}[' + x + ']'


    def stdev_of(x):
        return r'\sigma{}[' + x + ']'


    def mse(x):
        return 'MSE[' + x + ']'


    def cardinality(x):
        return r'\|' + x + r'\|'


    labels = {
        'nodeCount': Measure(r'$n$', 'nodes'),
        'harmonicCentrality[Mean]': Measure(f'${expected("H(x)")}$'),
        'meanNeighbors': Measure(f'${expected(cardinality("N"))}$', 'nodes'),
        'speed': Measure(r'$\|\vec{v}\|$', r'$m/s$'),
        'msqer@harmonicCentrality[Max]': Measure(r'$\max{(' + mse(centrality_label) + ')}$'),
        'msqer@harmonicCentrality[Min]': Measure(r'$\min{(' + mse(centrality_label) + ')}$'),
        'msqer@harmonicCentrality[Mean]': Measure(f'${expected(mse(centrality_label))}$'),
        'msqer@harmonicCentrality[StandardDeviation]': Measure(f'${stdev_of(mse(centrality_label))}$'),
        'org:protelis:tutorial:distanceTo[max]': Measure(r'$m$', 'max distance'),
        'org:protelis:tutorial:distanceTo[mean]': Measure(r'$m$', 'mean distance'),
        'org:protelis:tutorial:distanceTo[min]': Measure(r'$m$', ',min distance'),
    }


    def derivativeOrMeasure(variable_name):
        if variable_name.endswith('dt'):
            return labels.get(variable_name[:-2], Measure(variable_name)).derivative()
        return Measure(variable_name)


    def label_for(variable_name):
        return labels.get(variable_name, derivativeOrMeasure(variable_name)).description()


    def unit_for(variable_name):
        return str(labels.get(variable_name, derivativeOrMeasure(variable_name)))


    # Setup libraries
    np.set_printoptions(formatter={'float': floatPrecision.format})
    # Read the last time the data was processed, reprocess only if new data exists, otherwise just load
    import pickle
    import os

    if os.path.exists(directory):
        newestFileTime = max([os.path.getmtime(directory + '/' + file) for file in os.listdir(directory)], default=0.0)
        try:
            lastTimeProcessed = pickle.load(open('timeprocessed', 'rb'))
        except:
            lastTimeProcessed = -1
        shouldRecompute = not os.path.exists(".skip_data_process") and newestFileTime != lastTimeProcessed
        if not shouldRecompute:
            try:
                means = pickle.load(open(pickleOutput + '_mean', 'rb'))
                stdevs = pickle.load(open(pickleOutput + '_std', 'rb'))
            except:
                shouldRecompute = True
        if shouldRecompute:
            timefun = np.logspace if logarithmicTime else np.linspace
            means = {}
            stdevs = {}
            for experiment in experiments:
                # Collect all files for the experiment of interest
                import fnmatch

                allfiles = filter(lambda file: fnmatch.fnmatch(file, experiment + '_*.csv'), os.listdir(directory))
                allfiles = [directory + '/' + name for name in allfiles]
                allfiles.sort()
                # From the file name, extract the independent variables
                dimensions = {}
                for file in allfiles:
                    dimensions = mergeDicts(dimensions, extractCoordinates(file))
                dimensions = {k: sorted(v) for k, v in dimensions.items()}
                # Add time to the independent variables
                dimensions[timeColumnName] = range(0, timeSamples)
                # Compute the matrix shape
                shape = tuple(len(v) for k, v in dimensions.items())
                # Prepare the Dataset
                dataset = xr.Dataset()
                for k, v in dimensions.items():
                    dataset.coords[k] = v
                if len(allfiles) == 0:
                    print("WARNING: No data for experiment " + experiment)
                    means[experiment] = dataset
                    stdevs[experiment] = xr.Dataset()
                else:
                    varNames = extractVariableNames(allfiles[0])
                    for v in varNames:
                        if v != timeColumnName:
                            novals = np.ndarray(shape)
                            novals.fill(float('nan'))
                            dataset[v] = (dimensions.keys(), novals)
                    # Compute maximum and minimum time, create the resample
                    timeColumn = varNames.index(timeColumnName)
                    allData = {file: np.matrix(openCsv(file)) for file in allfiles}
                    computeMin = minTime is None
                    computeMax = maxTime is None
                    if computeMax:
                        maxTime = float('-inf')
                        for data in allData.values():
                            maxTime = max(maxTime, data[-1, timeColumn])
                    if computeMin:
                        minTime = float('inf')
                        for data in allData.values():
                            minTime = min(minTime, data[0, timeColumn])
                    timeline = timefun(minTime, maxTime, timeSamples)
                    # Resample
                    for file in allData:
                        #                    print(file)
                        allData[file] = convert(timeColumn, timeline, allData[file])
                    # Populate the dataset
                    for file, data in allData.items():
                        dataset[timeColumnName] = timeline
                        for idx, v in enumerate(varNames):
                            if v != timeColumnName:
                                darray = dataset[v]
                                experimentVars = extractCoordinates(file)
                                darray.loc[experimentVars] = data[:, idx].A1
                    # Fold the dataset along the seed variables, producing the mean and stdev datasets
                    mergingVariables = [seed for seed in seedVars if seed in dataset.coords]
                    means[experiment] = dataset.mean(dim=mergingVariables, skipna=True)
                    stdevs[experiment] = dataset.std(dim=mergingVariables, skipna=True)
            # Save the datasets
            pickle.dump(means, open(pickleOutput + '_mean', 'wb'), protocol=-1)
            pickle.dump(stdevs, open(pickleOutput + '_std', 'wb'), protocol=-1)
            pickle.dump(newestFileTime, open('timeprocessed', 'wb'))
    else:
        means = {experiment: xr.Dataset() for experiment in experiments}
        stdevs = {experiment: xr.Dataset() for experiment in experiments}

    # QUICK CHARTING

    import matplotlib
    import matplotlib.pyplot as plt
    import matplotlib.cm as cmx

    matplotlib.rcParams.update({'axes.titlesize': 12})
    matplotlib.rcParams.update({'axes.labelsize': 10})


    def make_line_chart(
            xdata,
            ydata,
            title=None,
            ylabel=None,
            xlabel=None,
            colors=None,
            linewidth=1,
            error_alpha=0.2,
            figure_size=(6, 4)
    ):
        fig = plt.figure(figsize=figure_size)
        ax = fig.add_subplot(1, 1, 1)
        ax.set_title(title)
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        #        ax.set_ylim(0)
        #        ax.set_xlim(min(xdata), max(xdata))
        index = 0
        for (label, (data, error)) in ydata.items():
            #            print(f'plotting {data}\nagainst {xdata}')
            lines = ax.plot(xdata, data, label=label, color=colors(index / (len(ydata) - 1)) if colors else None,
                            linewidth=linewidth)
            index += 1
            if error is not None:
                last_color = lines[-1].get_color()
                ax.fill_between(
                    xdata,
                    data + error,
                    data - error,
                    facecolor=last_color,
                    alpha=error_alpha,
                )
        return (fig, ax)


    def generate_all_charts(means, errors=None, basedir=''):
        viable_coords = {coord for coord in means.coords if means[coord].size > 1}
        for comparison_variable in viable_coords - {timeColumnName}:
            mergeable_variables = viable_coords - {timeColumnName, comparison_variable}
            for current_coordinate in mergeable_variables:
                merge_variables = mergeable_variables - {current_coordinate}
                merge_data_view = means.mean(dim=merge_variables, skipna=True)
                merge_error_view = errors.mean(dim=merge_variables, skipna=True)
                for current_coordinate_value in merge_data_view[current_coordinate].values:
                    beautified_value = beautifyValue(current_coordinate_value)
                    for current_metric in merge_data_view.data_vars:
                        title = f'{label_for(current_metric)} for diverse {label_for(comparison_variable)} when {label_for(current_coordinate)}={beautified_value}'
                        for withErrors in [True, False]:
                            fig, ax = make_line_chart(
                                title=title,
                                xdata=merge_data_view[timeColumnName],
                                xlabel=unit_for(timeColumnName),
                                ylabel=unit_for(current_metric),
                                ydata={
                                    beautifyValue(label): (
                                        merge_data_view.sel(selector)[current_metric],
                                        merge_error_view.sel(selector)[current_metric] if withErrors else 0
                                    )
                                    for label in merge_data_view[comparison_variable].values
                                    for selector in
                                    [{comparison_variable: label, current_coordinate: current_coordinate_value}]
                                },
                            )
                            ax.set_xlim(minTime, maxTime)
                            ax.legend()
                            fig.tight_layout()
                            by_time_output_directory = f'{output_directory}/{basedir}/{comparison_variable}'
                            Path(by_time_output_directory).mkdir(parents=True, exist_ok=True)
                            figname = f'{comparison_variable}_{current_metric}_{current_coordinate}_{beautified_value}{"_err" if withErrors else ""}'
                            for symbol in r".[]\/@:":
                                figname = figname.replace(symbol, '_')
                            fig.savefig(f'{by_time_output_directory}/{figname}.pdf')
                            plt.close(fig)


    for experiment in experiments:
        current_experiment_means = means[experiment]
        current_experiment_errors = stdevs[experiment]
        generate_all_charts(current_experiment_means, current_experiment_errors, basedir=f'{experiment}/all')

    # Custom charting

    import seaborn as sns
    import pandas as pd

    sns.color_palette("viridis", as_cmap=True)
    sns.set_palette("viridis")
    sns.set_style("white", { 'axes.grid': True })
    sns.set(font_scale=1.5)

    dataset = means['stable-gradient']

    # ------ Plot convergence time and errors
    convergence_time_error = dataset.mean(dim=['nodeCount'], skipna=True)
    convergence_time_error = convergence_time_error.sel({'scenario': 'monolithic'}) - convergence_time_error.sel({'scenario': 'offloaded'})
    convergence_time_error = convergence_time_error.apply(np.fabs)
    convergence_time_error = convergence_time_error.rename({'potential[sum]': 'error'})

    convergence_time_error = convergence_time_error.to_dataframe().reset_index()
    convergence_time_error = convergence_time_error.melt(
        id_vars=['time', 'surrogateFrequency'],
        var_name='delta_error',
        value_name='value',
    )
    print(convergence_time_error)

    pp = sns.catplot(
        data=convergence_time_error,
        x='surrogateFrequency',
        y='value',
        color='red',
        height=6,
        aspect=1.8,
        kind="point",
    )

    # pp.set_title('Average Error with Execution Frequencies')
    pp.fig.suptitle('Average Error with Execution Frequencies', fontsize=26)
    pp.set_ylabels('Error')
    pp.set_xlabels('Execution Frequency (Hz)')
    pp.tight_layout()
    pp.savefig(f'{output_directory}/convergence_time_error.pdf')

    # ------ Plot convergence time

    data_with_nodes = dataset.mean(dim=['surrogateFrequency'], skipna=True)
    data_with_nodes = data_with_nodes.to_dataframe().reset_index()
    data_with_nodes = data_with_nodes.melt(
        id_vars=['nodeCount', 'time', 'scenario'],
        var_name='metric',
        value_name='value'
    )

    potential_error = dataset.mean(dim=['surrogateFrequency'], skipna=True)
    potential_error = potential_error.sel({'scenario': 'monolithic'}) - potential_error.sel({'scenario': 'offloaded'})
    potential_error = potential_error.apply(np.fabs)
    potential_error = potential_error.rename({'potential[sum]': 'error'})

    # p = (
    #     sns.objects.Plot(data_with_nodes, x='time', y='value', color='scenario')
    #     .facet(col='nodeCount')
    #     .add(sns.objects.Lines())
    #     .scale(color='viridis')
    #     .layout(size=(20, 4))
    #     .share(x=True, y=False)
    #     .label(col='Node Count = ', y='Potential')
    # )
    # p.show()

    # ------ Plot convergence
    gradient_plot = sns.FacetGrid(
        data_with_nodes,
        col='nodeCount',
        sharey=False,
        sharex=True,
        height=5,
        aspect=1.5,
        legend_out=False
    )
    gradient_plot.map_dataframe(sns.lineplot, x='time', y='value', hue='scenario', palette='viridis')
    gradient_plot.fig.suptitle('Gradient Convergence', fontsize=26)
    gradient_plot.set_ylabels('Potential')
    gradient_plot.add_legend()
    gradient_plot.tight_layout()

    for ax in gradient_plot.axes.flatten():
        ax.ticklabel_format(style='sci', scilimits=(0, 0), axis='y')
    gradient_plot.savefig(f'{output_directory}/gradient_convergence.pdf')

    # ------ Plot convergence error
    error_plot = sns.FacetGrid(
        potential_error.to_dataframe().reset_index(),
        col='nodeCount',
        sharey=False,
        sharex=True,
        height=5,
        aspect=1.5,
        legend_out=False
    )
    error_plot.map_dataframe(sns.lineplot, x='time', y='error', color='red')
    error_plot.fig.suptitle('Gradient Convergence Error', fontsize=26)
    error_plot.set_ylabels('Error')
    error_plot.tight_layout()

    for ax in error_plot.axes.flatten():
        ax.ticklabel_format(style='sci', scilimits=(0, 0), axis='y')
    error_plot.savefig(f'{output_directory}/gradient_convergence_error.pdf')

    # ------ PLot SCR convergence regions
    scr_dataset = means['stable-scr']
    scr_data = scr_dataset.mean(dim=['surrogateFrequency'], skipna=True)

    scr_data = scr_data.to_dataframe().reset_index()
    scr_data = scr_data.melt(
        id_vars=['nodeCount', 'time', 'scenario', 'RegionCount'],
        var_name='metric',
        value_name='value'
    )

    scr_plot = sns.FacetGrid(
        scr_data,
        col='nodeCount',
        sharey=False,
        sharex=True,
        height=5,
        aspect=1.5,
        legend_out=False
    )
    scr_plot.map_dataframe(sns.lineplot, x='time', y='RegionCount', hue='scenario', palette='viridis')
    scr_plot.fig.suptitle('SCR Convergence', fontsize=26)
    scr_plot.set_ylabels('RegionsCount')
    scr_plot.add_legend()
    scr_plot.tight_layout()

    # for ax in scr_plot.axes.flatten():
    #     ax.ticklabel_format(style='sci', scilimits=(0, 0), axis='y')

    scr_plot.savefig(f'{output_directory}/scr_convergence.pdf')

    # ------ Plot SCR convergence error
    potential_error = scr_dataset.mean(dim=['surrogateFrequency'], skipna=True)
    potential_error = potential_error.sel({'scenario': 'monolithic'}) - potential_error.sel({'scenario': 'offloaded'})
    potential_error = potential_error.apply(np.fabs)
    potential_error = potential_error.rename({'potential[sum]': 'error'})

    error_plot = sns.FacetGrid(
        potential_error.to_dataframe().reset_index(),
        col='nodeCount',
        sharey=False,
        sharex=True,
        height=5,
        aspect=1.5,
        legend_out=False
    )
    error_plot.map_dataframe(sns.lineplot, x='time', y='error', color='red')
    error_plot.fig.suptitle('SCR Convergence Error', fontsize=26)
    error_plot.set_ylabels('Error')
    error_plot.tight_layout()

    for ax in error_plot.axes.flatten():
        ax.ticklabel_format(style='sci', scilimits=(0, 0), axis='y')

    error_plot.savefig(f'{output_directory}/scr_convergence_error.pdf')

    # ------ Plot Power consumption
    rescue_dataset = means['rescue']
    rescue_dataset_sdtdev = stdevs['rescue']
    rescue_dataset = rescue_dataset.mean(dim=['surrogateFrequency', 'nodeCount'], skipna=True)
    rescue_dataset_sdtdev = rescue_dataset_sdtdev.mean(dim=['surrogateFrequency', 'nodeCount'], skipna=True)
    rescue_dataset = rescue_dataset[['time', 'BatteryLevel[mean]', 'MessagesExchanged[sum]']]
    rescue_dataset_sdtdev = rescue_dataset_sdtdev[['BatteryLevel[mean]', 'MessagesExchanged[sum]']]
    rescue_dataset = rescue_dataset.to_dataframe().reset_index()
    rescue_dataset_sdtdev = rescue_dataset_sdtdev.to_dataframe().reset_index()

    power_data = rescue_dataset[["time", "scenario", "BatteryLevel[mean]"]]
    power_data_std = rescue_dataset_sdtdev[["time", "scenario", "BatteryLevel[mean]"]]
    messages_data = rescue_dataset[["time", "scenario", "MessagesExchanged[sum]"]]
    messages_data_std = rescue_dataset_sdtdev[["time", "scenario", "MessagesExchanged[sum]"]]

    power_data = power_data.melt(
        id_vars=['time', 'scenario'],
        var_name='metric',
        value_name='value'
    )
    power_data_std = power_data_std.melt(
        id_vars=['time', 'scenario'],
        var_name='metric',
        value_name='value'
    )
    messages_data = messages_data.melt(
        id_vars=['time', 'scenario'],
        var_name='metric',
        value_name='value'
    )
    messages_data_std = messages_data_std.melt(
        id_vars=['time', 'scenario'],
        var_name='metric',
        value_name='value'
    )

    fig, ax = plt.subplots(1, 2, figsize=(20, 5), layout='tight', sharey=False)
    sns.lineplot(
        data=power_data,
        ax=ax[0],
        x='time',
        y='value',
        hue='scenario',
        palette='viridis'
    )
    ax[0].set_ylabel('Battery Level [mAh]')

    sns.lineplot(
        data=messages_data,
        ax=ax[1],
        x='time',
        y='value',
        hue='scenario',
        palette='viridis'

    )
    ax[1].set_ylabel('Messages Exchanged')
    fig.suptitle('Power Consumption and Messages Exchanged', fontsize=26)
    fig.savefig(f'{output_directory}/power_consumption.pdf')

    plt.show()
